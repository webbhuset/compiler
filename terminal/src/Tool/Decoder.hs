{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Tool.Decoder
  ( run
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Build
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String as ES
import qualified Json.Encode as E
import qualified Json.String as Json
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import qualified Tool.Summary as Summary



-- RUN
--
--   elm tool decoder Some.Module.decoder            the JSON it accepts
--   elm tool decoder Some.Module.decoder --sample   a document it accepts
--
-- The decoder is read, not run: Json.Decode and the pipeline packages are
-- understood, decoders defined anywhere in the project are followed, and
-- whatever is left, like the function given to `andThen`, is looked into as
-- far as it goes and marked unknown where it stops.


run :: Bool -> String -> IO (Either Problem Output)
run sample target =
  case Project.parseValueName target of
    Nothing ->
      return (Left (BadName "a decoder like `Api.User.decoder`" target))

    Just (home, name) ->
      Project.withProject $ \_ _ modules ->
        let defs = definitions modules in
        case Map.lookup (home, name) defs of
          Nothing ->
            return $ Left $ NotFound ("a value in " ++ Module.toChars home) (N.toChars name) $
              Summary.suggest (N.toChars name) [ N.toChars n | (m, n) <- Map.keys defs, m == home ]

          Just body ->
            let shape = simplify (eval (Env defs Map.empty (Set.singleton (home, name))) body) in
            return $ Right $
              if sample
                then Output (render (sampleOf shape) ++ "\n") (sampleOf shape)
                else Output (schema 0 shape ++ "\n") (shapeToJson shape)



-- SHAPES


data Shape
  = SString
  | SInt
  | SFloat
  | SBool
  | SNull
  | SLiteral String
    -- a string that has to be this one, known from a `case` after `andThen`
  | SAny
    -- Json.Decode.value: anything at all
  | SSucceed
    -- accepts anything without looking at it
  | SFail
  | SList Shape
  | SDict Shape
  | SObject [(String, Shape, Bool)]
    -- field, its shape, and whether it has to be there
  | SIndex [(Int, Shape)]
  | SOneOf [Shape]
  | SAll [Shape]
    -- all of these at once, which is what mapN and andThen ask for
  | SMaybe Shape
    -- may fail without failing the whole decoder
  | SRecursive String
  | SUnknown String


simplify :: Shape -> Shape
simplify shape =
  case shape of
    SList s      -> SList (simplify s)
    SDict s      -> SDict (simplify s)
    SObject fs   -> SObject [ (f, simplify s, r) | (f, s, r) <- fs ]
    SIndex is    -> SIndex [ (i, simplify s) | (i, s) <- is ]
    SMaybe s     -> optional (simplify s)
    SOneOf ss    ->
      case filter (not . isFail) (map simplify ss) of
        [s] -> s
        ss' -> SOneOf ss'
    SAll ss      -> merge (map simplify ss)
    _            -> shape


isFail :: Shape -> Bool
isFail shape =
  case shape of
    SFail -> True
    _     -> False


-- `maybe (field "a" x)` makes the field optional.
optional :: Shape -> Shape
optional shape =
  case shape of
    SObject fs -> SObject [ (f, s, False) | (f, s, _) <- fs ]
    SAll ss    -> SAll (map optional ss)
    _          -> SOneOf [shape, SSucceed]


-- Decoders that all have to succeed on the same value. Alternatives are
-- pulled out, so `{ kind } & ({ r } | { w })` becomes
-- `{ kind, r } | { kind, w }`.
merge :: [Shape] -> Shape
merge shapes =
  case filter (not . isSucceed) shapes of
    ss | any isFail ss -> SFail
    [] -> SSucceed
    [s] -> s
    ss
      | (before, SOneOf options : after) <- break isOneOf ss ->
          SOneOf (filter (not . isFail) [ merge (before ++ [o] ++ after) | o <- options ])
    ss
      | all isObject ss -> SObject (mergeFields (concat [ fs | SObject fs <- ss ]))
      | all isIndex ss  -> SIndex (List.sortOn fst (concat [ is | SIndex is <- ss ]))
      | otherwise       -> SAll ss
  where
    isOneOf s = case s of SOneOf _ -> True; _ -> False
    isSucceed s = case s of SSucceed -> True; _ -> False
    isObject s = case s of SObject _ -> True; _ -> False
    isIndex s = case s of SIndex _ -> True; _ -> False


mergeFields :: [(String, Shape, Bool)] -> [(String, Shape, Bool)]
mergeFields fields =
  let
    names = List.nub [ f | (f, _, _) <- fields ]
    combine f =
      let same = [ (s, r) | (g, s, r) <- fields, g == f ] in
      (f, merge (map fst same), or (map snd same))
  in
  map combine names



-- EVALUATION


data Env =
  Env
    { _defs :: Map.Map (Module.Name, N.Name) Can.Expr
    , _locals :: Map.Map N.Name Can.Expr
    , _visiting :: Set.Set (Module.Name, N.Name)
    }


-- Top level definitions of the project, with the arguments of functions
-- dropped: a decoder that takes arguments is read with them unknown.
definitions :: [(FilePath, Build.Checked)] -> Map.Map (Module.Name, N.Name) Can.Expr
definitions modules =
  Map.fromList
    [ ((ModuleName._module (Can._name can), name), body)
    | (_, c) <- modules
    , let can = Build._checked_canonical c
    , def <- decls (Can._decls can)
    , let (name, body) = nameAndBody def
    ]
  where
    decls ds =
      case ds of
        Can.Declare d rest         -> d : decls rest
        Can.DeclareRec d dd rest   -> d : dd ++ decls rest
        Can.SaveTheEnvironment     -> []


nameAndBody :: Can.Def -> (N.Name, Can.Expr)
nameAndBody def =
  case def of
    Can.Def (A.At _ n) _ body          -> (n, body)
    Can.TypedDef (A.At _ n) _ _ body _ -> (n, body)


eval :: Env -> Can.Expr -> Shape
eval env expr =
  case spine expr [] of
    (A.At _ (Can.VarForeign (ModuleName.Canonical _ m) name _), args) ->
      apply env (Module.toChars m) (N.toChars name) args

    (A.At _ (Can.VarTopLevel (ModuleName.Canonical _ m) name), args) ->
      follow env (m, name) args

    (A.At _ (Can.VarLocal name), args) ->
      case Map.lookup name (_locals env) of
        Just e  -> eval env (applyArgs e args)
        Nothing -> SUnknown (N.toChars name)

    (A.At _ (Can.Let def body), []) ->
      let (n, e) = nameAndBody def in
      eval env { _locals = Map.insert n e (_locals env) } body

    (A.At _ (Can.LetRec defs body), []) ->
      eval env { _locals = Map.union (Map.fromList (map nameAndBody defs)) (_locals env) } body

    (A.At _ (Can.LetDestruct _ _ body), []) ->
      eval env body

    (A.At _ (Can.Case _ branches), []) ->
      SOneOf [ eval env b | Can.CaseBranch _ b <- branches ]

    (A.At _ (Can.If branches final), []) ->
      SOneOf (map (eval env . snd) branches ++ [eval env final])

    (A.At _ (Can.Lambda _ body), _) ->
      eval env body

    _ ->
      SUnknown "an expression this does not read"


-- A call flattened into its function and arguments, reading `x |> f` and
-- `f <| x` as `f x`.
spine :: Can.Expr -> [Can.Expr] -> (Can.Expr, [Can.Expr])
spine expr args =
  case expr of
    A.At _ (Can.Call f xs) ->
      spine f (xs ++ args)

    A.At _ (Can.Binop _ (ModuleName.Canonical _ m) name _ l r)
      | Module.toChars m == "Basics" && N.toChars name == "apR" -> spine r (l : args)
      | Module.toChars m == "Basics" && N.toChars name == "apL" -> spine l (r : args)

    _ ->
      (expr, args)


applyArgs :: Can.Expr -> [Can.Expr] -> Can.Expr
applyArgs f args =
  case args of
    [] -> f
    _  -> A.At (A.toRegion f) (Can.Call f args)


follow :: Env -> (Module.Name, N.Name) -> [Can.Expr] -> Shape
follow env key args =
  if Set.member key (_visiting env) then
    SRecursive (Module.toChars (fst key) ++ "." ++ N.toChars (snd key))
  else
    case Map.lookup key (_defs env) of
      Just body -> eval env { _visiting = Set.insert key (_visiting env), _locals = Map.empty } (applyArgs body args)
      Nothing   -> SUnknown (Module.toChars (fst key) ++ "." ++ N.toChars (snd key))


apply :: Env -> String -> String -> [Can.Expr] -> Shape
apply env home name args =
  let
    go = eval env
    arg i = if i < length args then go (args !! i) else SUnknown "a missing argument"
    strings i = if i < length args then stringList (args !! i) else []
    str i = if i < length args then stringLit (args !! i) else Nothing
  in
  case (home, name) of
    ("Json.Decode", "string")        -> SString
    ("Json.Decode", "int")           -> SInt
    ("Json.Decode", "float")         -> SFloat
    ("Json.Decode", "bool")          -> SBool
    ("Json.Decode", "value")         -> SAny
    ("Json.Decode", "null")          -> SNull
    ("Json.Decode", "succeed")       -> SSucceed
    ("Json.Decode", "fail")          -> SFail
    ("Json.Decode", "list")          -> SList (arg 0)
    ("Json.Decode", "array")         -> SList (arg 0)
    ("Json.Decode", "dict")          -> SDict (arg 0)
    ("Json.Decode", "keyValuePairs") -> SDict (arg 0)
    ("Json.Decode", "oneOrMore")     -> SList (arg 1)
    ("Json.Decode", "field")         -> maybe (SUnknown "a computed field name") (\f -> SObject [(f, arg 1, True)]) (str 0)
    ("Json.Decode", "at")            -> foldr (\f s -> SObject [(f, s, True)]) (arg 1) (strings 0)
    ("Json.Decode", "index")         -> maybe (SUnknown "a computed index") (\i -> SIndex [(i, arg 1)]) (intLit =<< safe args 0)
    ("Json.Decode", "maybe")         -> SMaybe (arg 0)
    ("Json.Decode", "nullable")      -> SOneOf [SNull, arg 0]
    ("Json.Decode", "oneOf")         -> SOneOf (maybe [] (map go) (listLit =<< safe args 0))
    ("Json.Decode", "lazy")          -> arg 0
    ("Json.Decode", "andThen")       -> andThen env (safe args 0) (arg 1)
    ("Json.Decode", "map")           -> arg 1
    ("Json.Decode", m) | List.isPrefixOf "map" m -> SAll (map go (drop 1 args))

    -- NoRedInk/elm-json-decode-pipeline
    ("Json.Decode.Pipeline", "required")    -> SAll [arg 2, maybe (SUnknown "a computed field name") (\f -> SObject [(f, arg 1, True)]) (str 0)]
    ("Json.Decode.Pipeline", "requiredAt")  -> SAll [arg 2, foldr (\f s -> SObject [(f, s, True)]) (arg 1) (strings 0)]
    ("Json.Decode.Pipeline", "optional")    -> SAll [arg 3, maybe (SUnknown "a computed field name") (\f -> SObject [(f, arg 1, False)]) (str 0)]
    ("Json.Decode.Pipeline", "optionalAt")  -> SAll [arg 3, optional (foldr (\f s -> SObject [(f, s, True)]) (arg 1) (strings 0))]
    ("Json.Decode.Pipeline", "hardcoded")   -> arg 1
    ("Json.Decode.Pipeline", "custom")      -> SAll [arg 1, arg 0]
    ("Json.Decode.Pipeline", "resolve")     -> arg 0

    -- elm-community/json-extra
    ("Json.Decode.Extra", "andMap")         -> SAll [arg 1, arg 0]
    ("Json.Decode.Extra", "optionalField")  -> maybe (SUnknown "a computed field name") (\f -> SObject [(f, arg 1, False)]) (str 0)

    _ -> SUnknown (home ++ "." ++ name)


-- `d |> andThen (\tag -> case tag of "a" -> ...)` is a decoder per string
-- the first one can give.
andThen :: Env -> Maybe Can.Expr -> Shape -> Shape
andThen env callback first =
  case callback of
    Just (A.At _ (Can.Lambda [A.At _ (Can.PVar x)] (A.At _ (Can.Case (A.At _ (Can.VarLocal y)) branches)))) | x == y ->
      SOneOf
        [ SAll [ maybe first (\lit -> literal lit first) (patternString p), eval env b ]
        | Can.CaseBranch p b <- branches
        ]

    Just f ->
      SAll [first, eval env f]

    Nothing ->
      first


patternString :: Can.Pattern -> Maybe String
patternString (A.At _ p) =
  case p of
    Can.PStr s -> Just (ES.toChars s)
    _          -> Nothing


-- Put a literal where the one string in a shape is.
literal :: String -> Shape -> Shape
literal lit shape =
  case shape of
    SString    -> SLiteral lit
    SObject fs | [_] <- [ () | (_, s, _) <- fs, hasString s ] -> SObject [ (f, if hasString s then literal lit s else s, r) | (f, s, r) <- fs ]
    _          -> shape
  where
    hasString s =
      case s of
        SString    -> True
        SObject fs -> any (\(_, x, _) -> hasString x) fs
        _          -> False


safe :: [a] -> Int -> Maybe a
safe xs i =
  case drop i xs of
    x : _ -> Just x
    []    -> Nothing


stringLit :: Can.Expr -> Maybe String
stringLit (A.At _ e) =
  case e of
    Can.Str s -> Just (ES.toChars s)
    _         -> Nothing


intLit :: Can.Expr -> Maybe Int
intLit (A.At _ e) =
  case e of
    Can.Int i -> Just i
    _         -> Nothing


listLit :: Can.Expr -> Maybe [Can.Expr]
listLit (A.At _ e) =
  case e of
    Can.List es -> Just es
    _           -> Nothing


stringList :: Can.Expr -> [String]
stringList e =
  maybe [] (concatMap (maybe [] (: []) . stringLit)) (listLit e)



-- SCHEMA


schema :: Int -> Shape -> String
schema indent shape =
  let pad n = replicate n ' ' in
  case shape of
    SString      -> "string"
    SLiteral l   -> show l
    SInt         -> "int"
    SFloat       -> "float"
    SBool        -> "bool"
    SNull        -> "null"
    SAny         -> "any"
    SSucceed     -> "any"
    SFail        -> "never"
    SList s      -> "[ " ++ schema indent s ++ " ]"
    SDict s      -> "{ string: " ++ schema indent s ++ " }"
    SObject []   -> "{}"
    SObject fs   ->
      "{\n"
      ++ List.intercalate ",\n" [ pad (indent + 2) ++ show f ++ (if r then "" else "?") ++ ": " ++ schema (indent + 2) s | (f, s, r) <- fs ]
      ++ "\n" ++ pad indent ++ "}"
    SIndex is    -> "[ " ++ List.intercalate ", " [ show i ++ ": " ++ schema indent s | (i, s) <- is ] ++ " ]"
    SOneOf ss    -> List.intercalate " | " (map (schema indent) ss)
    SAll ss      -> List.intercalate " & " (map (schema indent) ss)
    SMaybe s     -> schema indent s ++ "?"
    SRecursive n -> "<" ++ n ++ ">"
    SUnknown n   -> "<unknown: " ++ n ++ ">"


shapeToJson :: Shape -> E.Value
shapeToJson shape =
  let kind k extra = E.object (("kind" ==> E.chars k) : extra) in
  case shape of
    SString      -> kind "string" []
    SLiteral l   -> kind "literal" [ "value" ==> E.chars l ]
    SInt         -> kind "int" []
    SFloat       -> kind "float" []
    SBool        -> kind "bool" []
    SNull        -> kind "null" []
    SAny         -> kind "any" []
    SSucceed     -> kind "any" []
    SFail        -> kind "never" []
    SList s      -> kind "list" [ "of" ==> shapeToJson s ]
    SDict s      -> kind "dict" [ "of" ==> shapeToJson s ]
    SObject fs   -> kind "object" [ "fields" ==> E.list (\(f, s, r) -> E.object [ "name" ==> E.chars f, "required" ==> E.bool r, "shape" ==> shapeToJson s ]) fs ]
    SIndex is    -> kind "index" [ "items" ==> E.list (\(i, s) -> E.object [ "index" ==> E.int i, "shape" ==> shapeToJson s ]) is ]
    SOneOf ss    -> kind "oneOf" [ "options" ==> E.list shapeToJson ss ]
    SAll ss      -> kind "all" [ "parts" ==> E.list shapeToJson ss ]
    SMaybe s     -> kind "maybe" [ "of" ==> shapeToJson s ]
    SRecursive n -> kind "recursive" [ "name" ==> E.chars n ]
    SUnknown n   -> kind "unknown" [ "why" ==> E.chars n ]



-- SAMPLE


sampleOf :: Shape -> E.Value
sampleOf shape =
  case shape of
    SString      -> E.chars "string"
    SLiteral l   -> E.chars l
    SInt         -> E.int 0
    SFloat       -> E.number (read "0.5")
    SBool        -> E.bool True
    SNull        -> E.null
    SAny         -> E.null
    SSucceed     -> E.null
    SFail        -> E.null
    SList s      -> E.list sampleOf [s]
    SDict s      -> E.object [ (Json.fromChars "key", sampleOf s) ]
    SObject fs   -> E.object [ (Json.fromChars f, sampleOf s) | (f, s, _) <- fs ]
    SIndex is    -> E.list (\i -> maybe E.null sampleOf (lookup i is)) [0 .. maximum (0 : map fst is)]
    SOneOf ss    -> maybe E.null sampleOf (List.find (not . isFail) ss)
    SAll ss      -> case ss of { s : _ -> sampleOf s; [] -> E.null }
    SMaybe s     -> sampleOf s
    SRecursive _ -> E.null
    SUnknown _   -> E.null


render :: E.Value -> String
render value =
  BS_UTF8.toString (LBS.toStrict (B.toLazyByteString (E.encode value)))
