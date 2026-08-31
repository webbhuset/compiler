{-# LANGUAGE BangPatterns #-}
module Canonicalize.Overload
  ( addAbstracts
  , addConstrained
  , canonicalizeInstances
  , canonicalizeSignature
  , dictName
  , dispatchVar
  , dispatchKey
  , abstractVar
  , substitute
  )
  where


import qualified Data.Map.Strict as Map
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Expression as Expr
import qualified Canonicalize.Pattern as Pattern
import qualified AST.Utils.Type as Type
import qualified Canonicalize.Type as CanType
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as W



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a



-- OVERLOADS
--
-- An overload declaration is written qualified:
--
--     Order.compare : a -> a -> Order          -- abstract, no body
--
--     Order.compare : Card -> Card -> Order    -- a definition
--     Order.compare a b = ...
--
-- Without a body it declares that the name exists and says which argument
-- picks the definition. With a body it defines the name for one type.
--
-- The two are handled in separate passes because an abstract declaration has
-- to be in scope before anything can be defined for it, including definitions
-- in the same module.


isAbstract :: A.Located Src.Overload -> Bool
isAbstract (A.At _ (Src.Overload _ _ _ body)) =
  case body of
    Nothing -> True
    Just _  -> False



-- ABSTRACT DECLARATIONS
--
-- These belong to the module they are written in, so they go into the
-- environment under that module's own name and nothing has to be imported for
-- the definitions below to find them.


addAbstracts :: ModuleName.Raw -> [A.Located Src.Overload] -> Env.Env -> Result i w (Env.Env, Can.Overloads)
addAbstracts rawHome overloads env =
  do  abstracts <- traverse (addAbstract rawHome env) (filter isAbstract overloads)
      let table = Map.fromList abstracts
      Result.ok
        ( addToEnv rawHome table env
        , Can.Overloads table Map.empty Map.empty
        )


addAbstract :: ModuleName.Raw -> Env.Env -> A.Located Src.Overload -> Result i w (Can.OverloadName, Can.Annotation)
addAbstract rawHome env (A.At region (Src.Overload (A.At _ qual) (A.At _ name) srcType _)) =
  if qual /= rawHome then
    Result.throw (Error.OverloadForeignAbstract region qual name rawHome)
  else
    do  Src.Signature abstractType srcClauses <- Result.ok srcType
        noClauses region qual name srcClauses
        annotation@(Can.Forall _ tipe) <- CanType.toAnnotation env abstractType
        case dispatchArgument tipe of
          Just (Can.TVar _) ->
            Result.ok ((Env._home env, name), annotation)

          _ ->
            Result.throw (Error.OverloadAbstractNotDispatching region qual name)


-- An abstract declaration is what other clauses point at, so it cannot itself
-- ask for anything.
noClauses :: A.Region -> Name.Name -> Name.Name -> [A.Located Src.Constraint] -> Result i w ()
noClauses region qual name srcClauses =
  case srcClauses of
    []  -> Result.ok ()
    _:_ -> Result.throw (Error.WhereOnAbstract region qual name)


addToEnv :: ModuleName.Raw -> Map.Map Can.OverloadName Can.Annotation -> Env.Env -> Env.Env
addToEnv rawHome table env@(Env.Env home vs ts cs bs qvs qts qcs qos qcn cls) =
  if Map.null table then env else
  let
    !entries =
      Map.fromList
        [ (name, Env.Overload h annotation)
        | ((h, name), annotation) <- Map.toList table
        ]
  in
  Env.Env home vs ts cs bs qvs qts qcs (Map.insertWith Map.union rawHome entries qos) qcn cls



-- WHERE CLAUSES
--
-- A signature can say which overloads it needs, so that it can use them on
-- one of its own type variables:
--
--     sort : List a -> List a
--         where Ord.compare : a -> a -> Ordering
--
-- The clause has to be the abstract signature with its dispatch variable set
-- to one of this signature's, which is checked here rather than left to the
-- type checker so that the error can say what was expected.


canonicalizeSignature :: Env.Env -> Src.Signature -> Result i w (Can.Annotation, [Can.Constraint])
canonicalizeSignature env (Src.Signature srcType srcClauses) =
  do  annotation@(Can.Forall freeVars _) <- CanType.toAnnotation env srcType
      clauses <- addClauses env freeVars Map.empty (reverse srcClauses)
      Result.ok (annotation, Map.elems clauses)


addClauses
  :: Env.Env
  -> Can.FreeVars
  -> Map.Map (Can.OverloadName, Name.Name) Can.Constraint
  -> [A.Located Src.Constraint]
  -> Result i w (Map.Map (Can.OverloadName, Name.Name) Can.Constraint)
addClauses env freeVars seen srcClauses =
  case srcClauses of
    [] ->
      Result.ok seen

    A.At region (Src.Constraint (A.At _ qual) (A.At _ name) srcType) : rest ->
      case Map.lookup name =<< Map.lookup qual (Env._q_overloads env) of
        Nothing ->
          Result.throw (Error.OverloadNotDeclared region qual name)

        Just (Env.Overload ovHome abstract) ->
          do  tipe <- CanType.canonicalize env srcType
              case dispatchArgument tipe of
                Just (Can.TVar var) | Map.member var freeVars ->
                  if tipe /= substitute var abstract then
                    Result.throw $
                      Error.WhereWrongType region qual name var (substitute var abstract)
                  else
                    let key = ((ovHome, name), var) in
                    if Map.member key seen then
                      Result.throw (Error.WhereDuplicate region qual name var)
                    else
                      addClauses env freeVars
                        (Map.insert key (Can.Constraint (ovHome, name) tipe) seen)
                        rest

                _ ->
                  Result.throw $
                    Error.WhereNotDispatching region qual name (Map.keys freeVars)


-- The abstract signature with its own dispatch variable renamed to the one
-- this clause asks for, which is exactly what the clause has to say.
substitute :: Name.Name -> Can.Annotation -> Can.Type
substitute var (Can.Forall _ tipe) =
  case dispatchArgument tipe of
    Just (Can.TVar declared) ->
      rename (Map.singleton declared var) tipe

    _ ->
      tipe


rename :: Map.Map Name.Name Name.Name -> Can.Type -> Can.Type
rename subst tipe =
  case tipe of
    Can.TVar v            -> Can.TVar (Map.findWithDefault v v subst)
    Can.TLambda a b       -> Can.TLambda (rename subst a) (rename subst b)
    Can.TType h n as      -> Can.TType h n (map (rename subst) as)
    Can.TRecord fs ext    -> Can.TRecord (Map.map (renameField subst) fs) (fmap (\v -> Map.findWithDefault v v subst) ext)
    Can.TUnit             -> Can.TUnit
    Can.TTuple a b c      -> Can.TTuple (rename subst a) (rename subst b) (fmap (rename subst) c)
    Can.TAlias h n as t   -> Can.TAlias h n (map (fmap (rename subst)) as) (renameAliasType subst t)
    Can.TTagRow tags ext  -> Can.TTagRow (Map.map (map (rename subst)) tags) (fmap (\v -> Map.findWithDefault v v subst) ext)


renameField :: Map.Map Name.Name Name.Name -> Can.FieldType -> Can.FieldType
renameField subst (Can.FieldType index tipe) =
  Can.FieldType index (rename subst tipe)


renameAliasType :: Map.Map Name.Name Name.Name -> Can.AliasType -> Can.AliasType
renameAliasType subst aliasType =
  case aliasType of
    Can.Holey t  -> Can.Holey (rename subst t)
    Can.Filled t -> Can.Filled (rename subst t)


-- The type variable a clause dispatches on, which is one of the enclosing
-- signature's.
dispatchVar :: Can.Constraint -> Name.Name
dispatchVar (Can.Constraint _ tipe) =
  case dispatchArgument tipe of
    Just (Can.TVar var) -> var
    _                   -> Name.fromChars "_"


-- The type variable an abstract declaration dispatches on. Its instantiation
-- at a use site is the type that picks the definition.
abstractVar :: Can.Annotation -> Maybe Name.Name
abstractVar (Can.Forall _ tipe) =
  case dispatchArgument tipe of
    Just (Can.TVar var) -> Just var
    _                   -> Nothing


-- The name of the parameter a clause becomes. Unwritable in Elm, and derived
-- only from the clause, so the definition and every use site agree on it
-- without having to be told.
dictName :: Can.Constraint -> Name.Name
dictName constraint@(Can.Constraint (ovHome, ovName) _) =
  Name.sepBy 0x24 {-$-}
    (Name.sepBy 0x24 (flatten (ModuleName._module ovHome)) ovName)
    (dispatchVar constraint)



-- CONSTRAINED VALUES
--
-- Every value in scope whose signature has `where` clauses, so that a
-- reference to one can be given the overloads it asks for. The local ones are
-- collected before any body is canonicalized, since a body can call them.


addConstrained :: Env.Env -> [A.Located Src.Value] -> Result i w (Env.Env, Map.Map Can.OverloadName [Can.Constraint])
addConstrained env values =
  do  found <- traverse (toConstrained env) values
      let table = Map.fromList [ entry | Just entry <- found ]
      Result.ok (mergeConstrained table env, Map.map snd table)


toConstrained :: Env.Env -> A.Located Src.Value -> Result i w (Maybe (Can.OverloadName, (Can.Annotation, [Can.Constraint])))
toConstrained env (A.At _ (Src.Value (A.At _ name) _ _ maybeSignature)) =
  case maybeSignature of
    Just signature@(Src.Signature _ (_:_)) ->
      do  (annotation, clauses) <- canonicalizeSignature env signature
          Result.ok (Just ((Env._home env, name), (annotation, clauses)))

    _ ->
      Result.ok Nothing


mergeConstrained :: Map.Map Can.OverloadName (Can.Annotation, [Can.Constraint]) -> Env.Env -> Env.Env
mergeConstrained table (Env.Env home vs ts cs bs qvs qts qcs qos qcn cls) =
  Env.Env home vs ts cs bs qvs qts qcs qos (Map.union table qcn) cls



-- DEFINITIONS
--
-- A definition becomes an ordinary top-level value under a name no Elm
-- program can write, plus an entry recording which type it is for. Everything
-- downstream treats it as a normal definition.


canonicalizeInstances
  :: Env.Env
  -> Can.Overloads
  -> [A.Located Src.Overload]
  -> Result i [W.Warning] ([Can.Def], Can.Overloads)
canonicalizeInstances env (Can.Overloads abstracts instances constrained) overloads =
  do  found <-
        traverse (canonicalizeInstance env) $
          reverse (filter (not . isAbstract) overloads)

      table <- addInstances (Env._home env) Map.empty instances found
      let clauseTable =
            Map.fromList
              [ ((Env._home env, defName def), clauses)
              | (Found _ _ _ _ clauses, def) <- found
              , not (null clauses)
              ]
      Result.ok
        ( map snd found
        , Can.Overloads abstracts table (Map.union clauseTable constrained)
        )


data Found =
  Found A.Region Can.OverloadName Can.OverloadKey Can.Type [Can.Constraint]


type Instance =
  ( Found, Can.Def )


type Table =
  Map.Map Can.OverloadName (Map.Map Can.OverloadKey Can.Instance)


-- Fed in source order, so a duplicate is reported at the later definition and
-- points back at the first.
addInstances
  :: ModuleName.Canonical
  -> Map.Map (Can.OverloadName, Can.OverloadKey) A.Region
  -> Table
  -> [Instance]
  -> Result i w Table
addInstances home seen table found =
  case found of
    [] ->
      Result.ok table

    (Found region ovName key@(_, typeName) dispatched _, def) : rest ->
      case Map.lookup (ovName, key) seen of
        Just first ->
          Result.throw $
            Error.OverloadDuplicate
              (ModuleName._module (fst ovName)) (snd ovName) typeName first region

        Nothing ->
          let existing = Map.findWithDefault Map.empty ovName table in
          addInstances home
            (Map.insert (ovName, key) region seen)
            (Map.insert ovName
              (Map.insert key (Can.Instance (home, defName def) dispatched) existing) table)
            rest


defName :: Can.Def -> Name.Name
defName def =
  case def of
    Can.Def (A.At _ name) _ _          -> name
    Can.TypedDef (A.At _ name) _ _ _ _ -> name


canonicalizeInstance :: Env.Env -> A.Located Src.Overload -> Result i [W.Warning] Instance
canonicalizeInstance env (A.At region (Src.Overload (A.At qualRegion qual) (A.At _ name) srcType body)) =
  case Map.lookup name =<< Map.lookup qual (Env._q_overloads env) of
    Nothing ->
      Result.throw (Error.OverloadNotDeclared qualRegion qual name)

    Just (Env.Overload ovHome _) ->
      do  (Can.Forall freeVars tipe, clauses) <- canonicalizeSignature env srcType
          case dispatchArgument tipe of
            Nothing ->
              Result.throw (Error.OverloadInstanceNotDispatching region qual name)

            Just dispatched ->
              case dispatchKey dispatched of
                Nothing ->
                  Result.throw (Error.OverloadInstanceNotDispatching region qual name)

                Just key@(typeHome, _) ->
                  let home = Env._home env in
                  if home /= ovHome && home /= typeHome then
                    Result.throw (Error.OverloadNotOwned region qual name ovHome typeHome)
                  else
                    do  def <- toDef (Env.withClauses clauses env) region (mangle ovHome name key) name freeVars tipe body
                        Result.ok (Found region (ovHome, name) key dispatched clauses, def)


toDef
  :: Env.Env
  -> A.Region
  -> Name.Name
  -> Name.Name
  -> Can.FreeVars
  -> Can.Type
  -> Maybe ([Src.Pattern], Src.Expr)
  -> Result i [W.Warning] Can.Def
toDef env region mangled name freeVars tipe body =
  case body of
    Nothing ->
      error "canonicalizeInstance only ever looks at overloads that have a body"

    Just (srcArgs, srcBody) ->
      do  ((args, resultType), argBindings) <-
            Pattern.verify (Error.DPFuncArgs name) $
              Expr.gatherTypedArgs env name srcArgs tipe Index.first []

          newEnv <- Env.addLocals argBindings env

          (cbody, _) <-
            Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize newEnv srcBody)

          Result.ok (Can.TypedDef (A.At region mangled) freeVars args cbody resultType)



-- The mangled name only has to be unique within this module and impossible to
-- write in Elm, which `$` takes care of.
mangle :: ModuleName.Canonical -> Name.Name -> Can.OverloadKey -> Name.Name
mangle ovHome name (typeHome, typeName) =
  Name.sepBy 0x24 {-$-}
    (Name.sepBy 0x24 (flatten (ModuleName._module ovHome)) name)
    (Name.sepBy 0x24 (flatten (ModuleName._module typeHome)) typeName)


flatten :: ModuleName.Raw -> Name.Name
flatten raw =
  foldr1 (Name.sepBy 0x24 {-$-}) (Name.splitDots raw)



-- DISPATCH
--
-- Which definition a use site means is decided by the first argument, so both
-- an abstract signature and a definition have to be functions.


dispatchArgument :: Can.Type -> Maybe Can.Type
dispatchArgument tipe =
  case tipe of
    Can.TLambda arg _               -> Just arg
    Can.TAlias _ _ _ (Can.Holey t)  -> dispatchArgument t
    Can.TAlias _ _ _ (Can.Filled t) -> dispatchArgument t
    _                               -> Nothing


-- Aliases are transparent, so dispatch has to see through them. Otherwise
-- `type alias Name = String` could carry a second definition for String, and
-- which one ran would depend on how a signature happened to spell the type.
dispatchKey :: Can.Type -> Maybe Can.OverloadKey
dispatchKey tipe =
  case Type.iteratedDealias tipe of
    Can.TType home name _ -> Just (home, name)
    Can.TTuple _ _ maybeC -> Just (tupleKey maybeC)
    _                     -> Nothing


-- A tuple has no name of its own, so it gets one. Its home is elm/core's
-- Tuple, which means only the module that declares the overload can define
-- for it: a tuple is structural and belongs to nobody else.
tupleKey :: Maybe Can.Type -> Can.OverloadKey
tupleKey maybeC =
  ( ModuleName.tuple
  , case maybeC of
      Nothing -> Name.fromChars "Tuple2"
      Just _  -> Name.fromChars "Tuple3"
  )
