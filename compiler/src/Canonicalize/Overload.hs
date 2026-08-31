{-# LANGUAGE BangPatterns #-}
module Canonicalize.Overload
  ( addAbstracts
  , canonicalizeInstances
  )
  where


import qualified Data.Map.Strict as Map
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Expression as Expr
import qualified Canonicalize.Pattern as Pattern
import qualified Canonicalize.Type as Type
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
        , Can.Overloads table Map.empty
        )


addAbstract :: ModuleName.Raw -> Env.Env -> A.Located Src.Overload -> Result i w (Can.OverloadName, Can.Annotation)
addAbstract rawHome env (A.At region (Src.Overload (A.At _ qual) (A.At _ name) srcType _)) =
  if qual /= rawHome then
    Result.throw (Error.OverloadForeignAbstract region qual name rawHome)
  else
    do  annotation@(Can.Forall _ tipe) <- Type.toAnnotation env =<< Type.signatureType srcType
        case dispatchArgument tipe of
          Just (Can.TVar _) ->
            Result.ok ((Env._home env, name), annotation)

          _ ->
            Result.throw (Error.OverloadAbstractNotDispatching region qual name)


addToEnv :: ModuleName.Raw -> Map.Map Can.OverloadName Can.Annotation -> Env.Env -> Env.Env
addToEnv rawHome table env@(Env.Env home vs ts cs bs qvs qts qcs qos) =
  if Map.null table then env else
  let
    !entries =
      Map.fromList
        [ (name, Env.Overload h annotation)
        | ((h, name), annotation) <- Map.toList table
        ]
  in
  Env.Env home vs ts cs bs qvs qts qcs (Map.insertWith Map.union rawHome entries qos)



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
canonicalizeInstances env (Can.Overloads abstracts instances) overloads =
  do  found <-
        traverse (canonicalizeInstance env) $
          reverse (filter (not . isAbstract) overloads)

      table <- addInstances (Env._home env) Map.empty instances found
      Result.ok (map snd found, Can.Overloads abstracts table)


type Instance =
  ( (A.Region, Can.OverloadName, Can.OverloadKey, Name.Name), Can.Def )


type Table =
  Map.Map Can.OverloadName (Map.Map Can.OverloadKey Can.OverloadName)


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

    ((region, ovName, key, typeName), def) : rest ->
      case Map.lookup (ovName, key) seen of
        Just first ->
          Result.throw $
            Error.OverloadDuplicate
              (ModuleName._module (fst ovName)) (snd ovName) typeName first region

        Nothing ->
          let existing = Map.findWithDefault Map.empty ovName table in
          addInstances home
            (Map.insert (ovName, key) region seen)
            (Map.insert ovName (Map.insert key (home, defName def) existing) table)
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
      do  Can.Forall freeVars tipe <- Type.toAnnotation env =<< Type.signatureType srcType
          case dispatchKey =<< dispatchArgument tipe of
            Nothing ->
              Result.throw (Error.OverloadInstanceNotDispatching region qual name)

            Just key@(typeHome, typeName) ->
              let home = Env._home env in
              if home /= ovHome && home /= typeHome then
                Result.throw (Error.OverloadNotOwned region qual name ovHome typeHome)
              else
                do  def <- toDef env region (mangle ovHome name key) name freeVars tipe body
                    Result.ok ((region, (ovHome, name), key, typeName), def)


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


dispatchKey :: Can.Type -> Maybe Can.OverloadKey
dispatchKey tipe =
  case tipe of
    Can.TType home name _    -> Just (home, name)
    Can.TAlias home name _ _ -> Just (home, name)
    _                        -> Nothing
