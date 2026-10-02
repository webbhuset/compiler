{-# LANGUAGE MagicHash #-}
module Tool.Index
  ( Module(..)
  , Ref(..)
  , Kind(..)
  , kindToChars
  , Target
  , index
  , refsOf
  , start
  , end
  , namesIn
  , publicUnion
  )
  where


import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Module as ModuleName
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified Build
import qualified Elm.Interface as I
import qualified Elm.ModuleName as Canonical
import qualified Reporting.Annotation as A



-- INDEX
--
-- Every reference in the project to a top level name: where it is, which
-- declaration it is in, and what it names. `refs`, `unused`, `cases`,
-- `rename` and `move` are all questions about this table.


data Module =
  Module
    { _path :: FilePath
    , _checked :: Build.Checked
    , _refs :: [Ref]
    }


data Ref =
  Ref
    { _in :: Maybe String
      -- the top level declaration the reference is in
    , _region :: A.Region
    , _kind :: Kind
    , _target :: Target
    }


-- A top level name: its module, and the name itself.
type Target =
  (ModuleName.Name, String)


data Kind
  = Definition
  | Value
  | Construct
  | Match
  | Type
  | Operator
  | Exposing
  deriving (Eq)


kindToChars :: Kind -> String
kindToChars kind =
  case kind of
    Definition -> "definition"
    Value      -> "value"
    Construct  -> "constructor"
    Match      -> "pattern"
    Type       -> "type"
    Operator   -> "operator"
    Exposing   -> "exposing"


index :: [(FilePath, Build.Checked)] -> [Module]
index modules =
  [ Module path c (refsOf c) | (path, c) <- modules ]


refsOf :: Build.Checked -> [Ref]
refsOf (Build.Checked src can _ _ ifaces) =
  definitions src
  ++ exposingRefs src
  ++ declsRefs (Can._decls can)
  ++ typeRefs (typeEnv src ifaces) src


start :: A.Region -> (Int, Int)
start (A.Region s _) =
  A.toEditorRowCol s


end :: A.Region -> (Int, Int)
end (A.Region _ e) =
  A.toEditorRowCol e



-- DEFINITIONS


definitions :: Src.Module -> [Ref]
definitions src@(Src.Module _ _ _ _ values unions aliases tags _ _ effects) =
  let
    home = Src.getName src
    def r n = Ref (Just n) r Definition (home, n)
  in
  [ def r (N.toChars n) | A.At _ (Src.Value (A.At r n) _ _ _) <- values ]
  ++ [ def r (T.nameToChars n) | A.At _ (Src.Union (A.At r n) _ _) <- unions ]
  ++ [ Ref (Just (T.nameToChars u)) r Definition (home, N.toChars n) | A.At _ (Src.Union (A.At _ u) _ ctors) <- unions, (A.At r n, _) <- ctors ]
  ++ [ def r (T.nameToChars n) | A.At _ (Src.Alias (A.At r n) _ _) <- aliases ]
  ++ [ def r (N.toChars n) | A.At _ (Src.TagDecl (A.At r n) _) <- tags ]
  ++ [ def r (N.toChars n) | Src.Ports ports <- [effects], Src.Port (A.At r n) _ <- ports ]



-- EXPOSING


-- Names in the module's own `exposing` list, and in the `exposing` lists of
-- its imports.
exposingRefs :: Src.Module -> [Ref]
exposingRefs src@(Src.Module _ (A.At _ exports) _ imports _ _ _ _ _ _ _) =
  let
    inList home exposing =
      case exposing of
        Src.Open -> []
        Src.Explicit exposed ->
          [ Ref Nothing r Exposing (home, N.toChars n) | Src.Lower (A.At r n) <- exposed ]
          ++ [ Ref Nothing r Exposing (home, T.nameToChars n) | Src.Upper (A.At r n) _ <- exposed ]
          ++ [ Ref Nothing r Exposing (home, Op.toChars op) | Src.Operator r op <- exposed ]
  in
  inList (Src.getName src) exports
  ++ concat [ inList m exposing | Src.Import (A.At _ m) _ exposing _ <- imports ]



-- VALUES AND CONSTRUCTORS


declsRefs :: Can.Decls -> [Ref]
declsRefs decls =
  case decls of
    Can.Declare def rest         -> topDef def ++ declsRefs rest
    Can.DeclareRec def defs rest -> concatMap topDef (def:defs) ++ declsRefs rest
    Can.SaveTheEnvironment       -> []


topDef :: Can.Def -> [Ref]
topDef def =
  let
    name =
      case def of
        Can.Def (A.At _ n) _ _          -> n
        Can.TypedDef (A.At _ n) _ _ _ _ -> n
  in
  defRefs (Just (N.toChars name)) def


defRefs :: Maybe String -> Can.Def -> [Ref]
defRefs inDecl def =
  case def of
    Can.Def _ args body               -> concatMap (patternRefs inDecl) args ++ exprRefs inDecl body
    Can.TypedDef _ _ typedArgs body _ -> concatMap (patternRefs inDecl . fst) typedArgs ++ exprRefs inDecl body


exprRefs :: Maybe String -> Can.Expr -> [Ref]
exprRefs inDecl (A.At region e) =
  let
    go = exprRefs inDecl
    here (Canonical.Canonical _ home) name kind = [ Ref inDecl region kind (home, N.toChars name) ]
  in
  case e of
    Can.VarLocal _                          -> []
    Can.VarTopLevel home name               -> here home name Value
    Can.VarKernel _ _                       -> []
    Can.VarOverload _ (home, name) _        -> here home name Value
    Can.VarConstrained _ home name _ _      -> here home name Value
    Can.VarForeign home name _              -> here home name Value
    Can.VarCtor _ home name _ _             -> here home name Construct
    Can.VarTag home name _                  -> here home name Construct
    Can.VarDebug home name _                -> here home name Value
    Can.VarOperator op home _ _             -> [ Ref inDecl region Operator (Canonical._module home, Op.toChars op) ]
    Can.Chr _                               -> []
    Can.Str _                               -> []
    Can.Int _                               -> []
    Can.Float _                             -> []
    Can.List es                             -> concatMap go es
    Can.Negate a                            -> go a
    Can.Widen a                             -> go a
    Can.Binop op home _ _ a b               -> Ref inDecl region Operator (Canonical._module home, Op.toChars op) : go a ++ go b
    Can.Lambda args body                    -> concatMap (patternRefs inDecl) args ++ go body
    Can.Call f args                         -> go f ++ concatMap go args
    Can.If branches final                   -> concatMap (\(c, b) -> go c ++ go b) branches ++ go final
    Can.Let def body                        -> defRefs inDecl def ++ go body
    Can.LetRec defs body                    -> concatMap (defRefs inDecl) defs ++ go body
    Can.LetDestruct p a b                   -> patternRefs inDecl p ++ go a ++ go b
    Can.Case subject branches               -> go subject ++ concatMap (\(Can.CaseBranch p b) -> patternRefs inDecl p ++ go b) branches
    Can.Accessor _                          -> []
    Can.Access a _                          -> go a
    Can.Update _ a fields                   -> go a ++ concatMap (\(Can.FieldUpdate _ f) -> go f) (Map.elems fields)
    Can.Record fields                       -> concatMap go (Map.elems fields)
    Can.Unit                                -> []
    Can.Pair a b                            -> go a ++ go b
    Can.Triple a b c                        -> go a ++ go b ++ go c
    Can.Shader _ _                          -> []
    Can.Css _ _                             -> []


patternRefs :: Maybe String -> Can.Pattern -> [Ref]
patternRefs inDecl (A.At region p) =
  let go = patternRefs inDecl in
  case p of
    Can.PAnything      -> []
    Can.PVar _         -> []
    Can.PRecord _      -> []
    Can.PAlias a _     -> go a
    Can.PUnit          -> []
    Can.PPair a b      -> go a ++ go b
    Can.PTriple a b c  -> go a ++ go b ++ go c
    Can.PList ps       -> concatMap go ps
    Can.PCons a b      -> go a ++ go b
    Can.PBool _ _      -> []
    Can.PChr _         -> []
    Can.PStr _         -> []
    Can.PInt _         -> []
    Can.PCtor home _ _ name _ args ->
      Ref inDecl region Match (Canonical._module home, N.toChars name) : concatMap (go . Can._arg) args
    Can.PTag home name _ args ->
      Ref inDecl region Match (Canonical._module home, N.toChars name) : concatMap go args



-- TYPES
--
-- The canonical AST keeps no regions for types, so type references are read
-- from the source and resolved by hand: a local type, then the imports.


data TypeEnv =
  TypeEnv
    { _home :: ModuleName.Name
    , _locals :: [String]
    , _imports :: [(ModuleName.Name, String, Src.Exposing)]
      -- module, how it is qualified, what it exposes unqualified
    , _ifaces :: Map.Map ModuleName.Name I.Interface
    }


typeEnv :: Src.Module -> Map.Map ModuleName.Name I.Interface -> TypeEnv
typeEnv src@(Src.Module _ _ _ imports _ unions aliases _ _ _ _) ifaces =
  TypeEnv
    { _home = Src.getName src
    , _locals =
        [ T.nameToChars n | A.At _ (Src.Union (A.At _ n) _ _) <- unions ]
        ++ [ T.nameToChars n | A.At _ (Src.Alias (A.At _ n) _ _) <- aliases ]
    , _imports =
        [ (name, maybe (ModuleName.toChars name) ModuleName.prefixToChars alias, exposing)
        | Src.Import (A.At _ name) alias exposing _ <- imports
        ]
    , _ifaces = ifaces
    }


resolve :: TypeEnv -> Maybe String -> String -> Maybe ModuleName.Name
resolve env qualifier name =
  case qualifier of
    Just q ->
      Maybe.listToMaybe [ m | (m, as, _) <- _imports env, as == q ]

    Nothing ->
      if elem name (_locals env) then Just (_home env) else
        Maybe.listToMaybe [ m | (m, _, exposing) <- _imports env, exposes env m exposing name ]


exposes :: TypeEnv -> ModuleName.Name -> Src.Exposing -> String -> Bool
exposes env m exposing name =
  case exposing of
    Src.Open ->
      case Map.lookup m (_ifaces env) of
        Just i  -> any ((== name) . T.nameToChars) (Map.keys (I._unions i) ++ Map.keys (I._aliases i))
        Nothing -> False

    Src.Explicit exposed ->
      or [ T.nameToChars n == name | Src.Upper (A.At _ n) _ <- exposed ]


typeRefs :: TypeEnv -> Src.Module -> [Ref]
typeRefs env src =
  [ Ref inDecl r Type (home, n)
  | (inDecl, t) <- moduleTypes src
  , (r, qualifier, n) <- typeNames t
  , Just home <- [resolve env qualifier n]
  ]


-- Every type written in a module, with the declaration it is in:
-- signatures, also inside `let`, the arguments of constructors, aliases,
-- ports, and overloads.
moduleTypes :: Src.Module -> [(Maybe String, Src.Type)]
moduleTypes (Src.Module _ _ _ _ values unions aliases _ overloads _ effects) =
  concat
    [ [ (Just (N.toChars n), t) | A.At _ (Src.Value (A.At _ n) _ body sig) <- values, t <- maybe [] signatureTypes sig ++ exprTypes body ]
    , [ (Just (T.nameToChars n), t) | A.At _ (Src.Union (A.At _ n) _ ctors) <- unions, (_, ts) <- ctors, t <- ts ]
    , [ (Just (T.nameToChars n), t) | A.At _ (Src.Alias (A.At _ n) _ t) <- aliases ]
    , [ (Just (N.toChars n), t) | Src.Ports ports <- [effects], Src.Port (A.At _ n) t <- ports ]
    , [ (Nothing, t) | o <- overloads, t <- overloadTypes o ]
    ]


overloadTypes :: A.Located Src.Overload -> [Src.Type]
overloadTypes (A.At _ overload) =
  case overload of
    Src.Abstract _ sig              -> signatureTypes sig
    Src.DefineFor _ _ sig _ body    -> signatureTypes sig ++ exprTypes body


signatureTypes :: Src.Signature -> [Src.Type]
signatureTypes (Src.Signature t constraints) =
  t : [ ct | A.At _ (Src.Constraint _ _ ct) <- constraints ]


exprTypes :: Src.Expr -> [Src.Type]
exprTypes (A.At _ e) =
  case e of
    Src.Let defs body       -> concatMap defTypes defs ++ exprTypes body
    Src.List es             -> concatMap exprTypes es
    Src.Negate a            -> exprTypes a
    Src.Binops pairs final  -> concatMap (exprTypes . fst) pairs ++ exprTypes final
    Src.Lambda _ body       -> exprTypes body
    Src.Call f args         -> concatMap exprTypes (f : args)
    Src.If branches final   -> concatMap (\(c, b) -> exprTypes c ++ exprTypes b) branches ++ exprTypes final
    Src.Case subject bs     -> exprTypes subject ++ concatMap (exprTypes . snd) bs
    Src.Access a _          -> exprTypes a
    Src.Update _ fields     -> concatMap (exprTypes . snd) fields
    Src.Record fields       -> concatMap (exprTypes . snd) fields
    Src.Tuple a b cs        -> concatMap exprTypes (a : b : cs)
    _                       -> []


defTypes :: A.Located Src.Def -> [Src.Type]
defTypes (A.At _ def) =
  case def of
    Src.Define _ _ body sig -> maybe [] signatureTypes sig ++ exprTypes body
    Src.Destruct _ body     -> exprTypes body


-- Every type name in a type, with where it is and how it is qualified.
typeNames :: Src.Type -> [(A.Region, Maybe String, String)]
typeNames (A.At _ t) =
  case t of
    Src.TLambda a b              -> typeNames a ++ typeNames b
    Src.TVar _                   -> []
    Src.TType r n args           -> (r, Nothing, T.nameToChars n) : concatMap typeNames args
    Src.TTypeQual r q n args     -> (r, Just (ModuleName.prefixToChars q), T.nameToChars n) : concatMap typeNames args
    Src.TRecord fields _         -> concatMap (typeNames . snd) fields
    Src.TUnit                    -> []
    Src.TTuple a b cs            -> concatMap typeNames (a : b : cs)
    Src.TTagRow entries _        -> [ x | Src.TagEntry _ _ _ args <- entries, a <- args, x <- typeNames a ]



-- NAMES


-- The top level names of a module, from the project or from an interface.
namesIn :: ModuleName.Name -> [(FilePath, Build.Checked)] -> Maybe [String]
namesIn home modules =
  case [ c | (_, c) <- modules, Src.getName (Build._checked_source c) == home ] of
    c : _ ->
      let can = Build._checked_canonical c in
      Just $
        map N.toChars (Map.keys (Build._checked_annotations c))
        ++ map T.nameToChars (Map.keys (Can._unions can))
        ++ map T.nameToChars (Map.keys (Can._aliases can))
        ++ [ N.toChars ctor | u <- Map.elems (Can._unions can), Can.Ctor ctor _ _ _ <- Can._u_alts u ]

    [] ->
      case [ i | (_, c) <- modules, Just i <- [Map.lookup home (Build._checked_interfaces c)] ] of
        i : _ ->
          Just $
            map N.toChars (Map.keys (I._values i))
            ++ map T.nameToChars (Map.keys (I._unions i))
            ++ map T.nameToChars (Map.keys (I._aliases i))
            ++ [ N.toChars ctor | iu <- Map.elems (I._unions i), Just u <- [publicUnion iu], Can.Ctor ctor _ _ _ <- Can._u_alts u ]

        [] ->
          Nothing


publicUnion :: I.Union -> Maybe Can.Union
publicUnion iu =
  case iu of
    I.OpenUnion u    -> Just u
    I.ClosedUnion _  -> Nothing
    I.PrivateUnion _ -> Nothing
