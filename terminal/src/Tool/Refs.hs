{-# LANGUAGE MagicHash, OverloadedStrings #-}
module Tool.Refs
  ( run
  )
  where


import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Build
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import qualified Tool.Summary as Summary



-- RUN
--
--   elm tool refs Some.Module.value   where a value is defined and used
--   elm tool refs Some.Module.Name    a type, or a constructor, or both
--
-- One line per place, like grep:
--
--   src/Page/Home.elm:42:5: update msg model =
--   src/Main.elm:61:22:         Home.update subMsg home


run :: String -> IO (Either Problem Output)
run target =
  case parseTarget target of
    Nothing ->
      return (Left (BadName "a value like `Page.Home.view` or a type like `Page.Home.Model`" target))

    Just (home, name) ->
      Project.withProject $ \root modules ->
        case checkExists home name modules of
          Just suggestions ->
            return $ Left $ NotFound ("anything in " ++ Module.toChars home) name suggestions

          Nothing ->
            do  places <- traverse (withLines root) (concatMap (findIn home name) modules)
                let sorted = List.sortOn (\p -> (_kind p /= Definition, _path p, _line p, _column p)) (concat places)
                return $ Right $ Output (concatMap toText sorted) (E.list toJson sorted)


parseTarget :: String -> Maybe (Module.Name, String)
parseTarget string =
  case Project.parseValueName string of
    Just (home, name) ->
      Just (home, N.toChars name)

    Nothing ->
      case break (== '.') (reverse string) of
        (revName@(_:_), '.' : revHome) ->
          do  home <- Project.parseModuleName (reverse revHome)
              _ <- Project.parseModuleName (reverse revName)
              Just (home, reverse revName)

        _ ->
          Nothing



-- PLACES


data Place =
  Place
    { _path :: FilePath
    , _line :: Int
    , _column :: Int
    , _endLine :: Int
    , _endColumn :: Int
    , _kind :: Kind
    , _source :: String
    }


data Kind
  = Definition
  | Value
  | Constructor
  | Type
  | Exposing
  deriving (Eq)


kindToChars :: Kind -> String
kindToChars kind =
  case kind of
    Definition  -> "definition"
    Value       -> "value"
    Constructor -> "constructor"
    Type        -> "type"
    Exposing    -> "exposing"


toPlace :: FilePath -> Kind -> A.Region -> Place
toPlace path kind (A.Region start end) =
  let
    (sr, sc) = A.toEditorRowCol start
    (er, ec) = A.toEditorRowCol end
  in
  Place path sr sc er ec kind ""


-- Fill in the source line of each place, which is what makes the output
-- readable without opening the files.
withLines :: FilePath -> (FilePath, [Place]) -> IO [Place]
withLines root (path, places) =
  if null places then return [] else
  do  source <- File.readUtf8 (root ++ "/" ++ path)
      let ls = lines (BS_UTF8.toString source)
      return [ p { _source = trim (Maybe.fromMaybe "" (index ls (_line p - 1))) } | p <- places ]


index :: [a] -> Int -> Maybe a
index xs n =
  case drop n xs of
    x:_ | n >= 0 -> Just x
    _            -> Nothing


trim :: String -> String
trim =
  List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace


toText :: Place -> String
toText p =
  _path p ++ ":" ++ show (_line p) ++ ":" ++ show (_column p) ++ ": " ++ _source p ++ "\n"


toJson :: Place -> E.Value
toJson p =
  E.object
    [ "path" ==> E.chars (_path p)
    , "line" ==> E.int (_line p)
    , "column" ==> E.int (_column p)
    , "endLine" ==> E.int (_endLine p)
    , "endColumn" ==> E.int (_endColumn p)
    , "kind" ==> E.chars (kindToChars (_kind p))
    , "text" ==> E.chars (_source p)
    ]



-- EXISTS


-- Nothing when the name exists, otherwise the names that are close.
checkExists :: Module.Name -> String -> [(FilePath, Build.Checked)] -> Maybe [String]
checkExists home name modules =
  let
    candidates = Maybe.fromMaybe [] (namesIn home modules)
  in
  if elem name candidates then Nothing else Just (Summary.suggest name candidates)


namesIn :: Module.Name -> [(FilePath, Build.Checked)] -> Maybe [String]
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



-- FIND


findIn :: Module.Name -> String -> (FilePath, Build.Checked) -> [(FilePath, [Place])]
findIn home name (path, Build.Checked src can _ _ ifaces) =
  let
    isHome canonical = ModuleName._module canonical == home
    matches canonical n = isHome canonical && N.toChars n == name

    values = [ toPlace path k r | (r, k) <- exprRefs matches (Can._decls can) ]
    types = [ toPlace path Type r | r <- typeRefs (typeEnv src ifaces) home name src ]
    defs = if Src.getName src == home then map (toPlace path Definition) (definitions name src) else []
    exposed = map (toPlace path Exposing) (exposingRefs home name src)
  in
  [(path, defs ++ exposed ++ values ++ types)]



-- VALUES AND CONSTRUCTORS


exprRefs :: (ModuleName.Canonical -> N.Name -> Bool) -> Can.Decls -> [(A.Region, Kind)]
exprRefs matches decls =
  case decls of
    Can.Declare def rest         -> defRefs matches def ++ exprRefs matches rest
    Can.DeclareRec def defs rest -> concatMap (defRefs matches) (def:defs) ++ exprRefs matches rest
    Can.SaveTheEnvironment       -> []


defRefs :: (ModuleName.Canonical -> N.Name -> Bool) -> Can.Def -> [(A.Region, Kind)]
defRefs matches def =
  case def of
    Can.Def _ args body            -> concatMap (patternRefs matches) args ++ expr matches body
    Can.TypedDef _ _ typedArgs body _ -> concatMap (patternRefs matches . fst) typedArgs ++ expr matches body


expr :: (ModuleName.Canonical -> N.Name -> Bool) -> Can.Expr -> [(A.Region, Kind)]
expr matches (A.At region e) =
  let
    go = expr matches
    here home name kind = [ (region, kind) | matches home name ]
  in
  case e of
    Can.VarLocal _                          -> []
    Can.VarTopLevel home name               -> here home name Value
    Can.VarKernel _ _                       -> []
    Can.VarOverload _ (home, name) _        -> here home name Value
    Can.VarConstrained _ home name _ _      -> here home name Value
    Can.VarForeign home name _              -> here home name Value
    Can.VarCtor _ home name _ _             -> here home name Constructor
    Can.VarTag home name _                  -> here home name Constructor
    Can.VarDebug home name _                -> here home name Value
    Can.VarOperator _ _ _ _                 -> []
    Can.Chr _                               -> []
    Can.Str _                               -> []
    Can.Int _                               -> []
    Can.Float _                             -> []
    Can.List es                             -> concatMap go es
    Can.Negate a                            -> go a
    Can.Widen a                             -> go a
    Can.Binop _ _ _ _ a b                   -> go a ++ go b
    Can.Lambda args body                    -> concatMap (patternRefs matches) args ++ go body
    Can.Call f args                         -> go f ++ concatMap go args
    Can.If branches final                   -> concatMap (\(c, b) -> go c ++ go b) branches ++ go final
    Can.Let def body                        -> defRefs matches def ++ go body
    Can.LetRec defs body                    -> concatMap (defRefs matches) defs ++ go body
    Can.LetDestruct p a b                   -> patternRefs matches p ++ go a ++ go b
    Can.Case subject branches               -> go subject ++ concatMap (\(Can.CaseBranch p b) -> patternRefs matches p ++ go b) branches
    Can.Accessor _                          -> []
    Can.Access a _                          -> go a
    Can.Update _ a fields                   -> go a ++ concatMap (\(Can.FieldUpdate _ f) -> go f) (Map.elems fields)
    Can.Record fields                       -> concatMap go (Map.elems fields)
    Can.Unit                                -> []
    Can.Pair a b                            -> go a ++ go b
    Can.Triple a b c                        -> go a ++ go b ++ go c
    Can.Shader _ _                          -> []
    Can.Css _ _                             -> []


patternRefs :: (ModuleName.Canonical -> N.Name -> Bool) -> Can.Pattern -> [(A.Region, Kind)]
patternRefs matches (A.At region p) =
  let go = patternRefs matches in
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
      [ (region, Constructor) | matches home name ] ++ concatMap (go . Can._arg) args
    Can.PTag home name _ args ->
      [ (region, Constructor) | matches home name ] ++ concatMap go args



-- DEFINITIONS


definitions :: String -> Src.Module -> [A.Region]
definitions name (Src.Module _ _ _ _ values unions aliases tags _ _ effects) =
  [ r | A.At _ (Src.Value (A.At r n) _ _ _) <- values, N.toChars n == name ]
  ++ [ r | A.At _ (Src.Union (A.At r n) _ _) <- unions, T.nameToChars n == name ]
  ++ [ r | A.At _ (Src.Union _ _ ctors) <- unions, (A.At r n, _) <- ctors, N.toChars n == name ]
  ++ [ r | A.At _ (Src.Alias (A.At r n) _ _) <- aliases, T.nameToChars n == name ]
  ++ [ r | A.At _ (Src.TagDecl (A.At r n) _) <- tags, N.toChars n == name ]
  ++ [ r | Src.Ports ports <- [effects], Src.Port (A.At r n) _ <- ports, N.toChars n == name ]



-- EXPOSING


-- The name in the module's own `exposing` list, and in the `exposing` lists
-- of imports of it.
exposingRefs :: Module.Name -> String -> Src.Module -> [A.Region]
exposingRefs home name src@(Src.Module _ (A.At _ exports) _ imports _ _ _ _ _ _ _) =
  let
    inList exposing =
      case exposing of
        Src.Open -> []
        Src.Explicit exposed ->
          [ r | Src.Lower (A.At r n) <- exposed, N.toChars n == name ]
          ++ [ r | Src.Upper (A.At r n) _ <- exposed, T.nameToChars n == name ]
  in
  (if Src.getName src == home then inList exports else [])
  ++ concat [ inList exposing | Src.Import (A.At _ m) _ exposing _ <- imports, m == home ]



-- TYPES
--
-- The canonical AST keeps no regions for types, so type references are read
-- from the source and resolved by hand: a local type, then the imports.


data TypeEnv =
  TypeEnv
    { _home :: Module.Name
    , _locals :: [String]
    , _imports :: [(Module.Name, String, Src.Exposing)]
      -- module, how it is qualified, what it exposes unqualified
    , _ifaces :: Map.Map Module.Name I.Interface
    }


typeEnv :: Src.Module -> Map.Map Module.Name I.Interface -> TypeEnv
typeEnv src@(Src.Module _ _ _ imports _ unions aliases _ _ _ _) ifaces =
  TypeEnv
    { _home = Src.getName src
    , _locals =
        [ T.nameToChars n | A.At _ (Src.Union (A.At _ n) _ _) <- unions ]
        ++ [ T.nameToChars n | A.At _ (Src.Alias (A.At _ n) _ _) <- aliases ]
    , _imports =
        [ (name, maybe (Module.toChars name) Module.prefixToChars alias, exposing)
        | Src.Import (A.At _ name) alias exposing _ <- imports
        ]
    , _ifaces = ifaces
    }


resolve :: TypeEnv -> Maybe String -> String -> Maybe Module.Name
resolve env qualifier name =
  case qualifier of
    Just q ->
      Maybe.listToMaybe [ m | (m, as, _) <- _imports env, as == q ]

    Nothing ->
      if elem name (_locals env) then Just (_home env) else
        Maybe.listToMaybe [ m | (m, _, exposing) <- _imports env, exposes env m exposing name ]


exposes :: TypeEnv -> Module.Name -> Src.Exposing -> String -> Bool
exposes env m exposing name =
  case exposing of
    Src.Open ->
      case Map.lookup m (_ifaces env) of
        Just i  -> any ((== name) . T.nameToChars) (Map.keys (I._unions i) ++ Map.keys (I._aliases i))
        Nothing -> False

    Src.Explicit exposed ->
      or [ T.nameToChars n == name | Src.Upper (A.At _ n) _ <- exposed ]


typeRefs :: TypeEnv -> Module.Name -> String -> Src.Module -> [A.Region]
typeRefs env home name src =
  [ r
  | (r, qualifier, n) <- concatMap typeNames (moduleTypes src)
  , n == name
  , resolve env qualifier n == Just home
  ]


-- Every type written in a module: signatures, also inside `let`, the
-- arguments of constructors, aliases, ports, and overloads.
moduleTypes :: Src.Module -> [Src.Type]
moduleTypes (Src.Module _ _ _ _ values unions aliases _ overloads _ effects) =
  concat
    [ concatMap (\(A.At _ (Src.Value _ _ body sig)) -> maybe [] signatureTypes sig ++ exprTypes body) values
    , [ t | A.At _ (Src.Union _ _ ctors) <- unions, (_, ts) <- ctors, t <- ts ]
    , [ t | A.At _ (Src.Alias _ _ t) <- aliases ]
    , [ t | Src.Ports ports <- [effects], Src.Port _ t <- ports ]
    , concatMap overloadTypes overloads
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
    Src.Let defs body ->
      concatMap defTypes defs ++ exprTypes body

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
    Src.TTypeQual r q n args     -> (r, Just (Module.prefixToChars q), T.nameToChars n) : concatMap typeNames args
    Src.TRecord fields _         -> concatMap (typeNames . snd) fields
    Src.TUnit                    -> []
    Src.TTuple a b cs            -> concatMap typeNames (a : b : cs)
    Src.TTagRow entries _        -> [ x | Src.TagEntry _ _ _ args <- entries, a <- args, x <- typeNames a ]
