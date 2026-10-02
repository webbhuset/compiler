{-# LANGUAGE MagicHash, OverloadedStrings #-}
module Tool.At
  ( run
  )
  where


import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.Read (readMaybe)

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified Build
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import qualified Type.Solve as Solve
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import qualified Tool.Render as Render



-- RUN
--
--   elm tool at src/Page/Home.elm:42:17
--
-- The type of the innermost expression at a position, and every local name
-- in scope there with its type:
--
--   Counter.update msg app.counter : Model
--
--   msg : Msg
--   app : App


run :: String -> IO (Either Problem Output)
run target =
  case parseTarget target of
    Nothing ->
      return (Left (BadName "a position like `src/Main.elm:42:17`" target))

    Just (path, pos) ->
      Project.withProbe path $ \source (Build.Probed src can _ probes) ->
        let
          ns = Render.names src
          render ann = Render.oneLine (Render.annotation ns (Render.normalize ann))
          contains (Solve.Probed r _ _) = inside pos r
          innermost = List.sortOn (\(Solve.Probed r _ _) -> size r) (filter contains probes)
          scope = scopeAt pos (Can._decls can)
          typed = [ (name, ann) | (name, region) <- scope, Just ann <- [bindingType probes name region] ]
          snippet r = excerpt (BS_UTF8.toString source) r
        in
        case innermost of
          [] ->
            return $ Left $ NothingAt target

          Solve.Probed region _ ann : _ ->
            return $ Right $ Output
              ( snippet region ++ " : " ++ render ann ++ "\n"
                ++ (if null typed then "" else "\n" ++ concatMap (\(n, a) -> N.toChars n ++ " : " ++ render a ++ "\n") typed)
              )
              ( E.object
                  [ "expression" ==> E.chars (snippet region)
                  , "type" ==> E.chars (render ann)
                  , "region" ==> regionToJson region
                  , "scope" ==> E.list (\(n, a) -> E.object [ "name" ==> E.chars (N.toChars n), "type" ==> E.chars (render a) ]) typed
                  ]
              )


-- `path:line:column`, both counting from one.
parseTarget :: String -> Maybe (FilePath, (Int, Int))
parseTarget string =
  case reverse (splitOn ':' string) of
    col : line : revPath@(_:_) ->
      do  l <- readMaybe line
          c <- readMaybe col
          Just (List.intercalate ":" (reverse revPath), (l, c))

    _ ->
      Nothing


splitOn :: Char -> String -> [String]
splitOn sep string =
  case break (== sep) string of
    (chunk, [])     -> [chunk]
    (chunk, _:rest) -> chunk : splitOn sep rest



-- REGIONS


type Pos = (Int, Int)


bounds :: A.Region -> (Pos, Pos)
bounds (A.Region start end) =
  (A.toEditorRowCol start, A.toEditorRowCol end)


-- Regions end just past their last character.
inside :: Pos -> A.Region -> Bool
inside pos region =
  let (start, end) = bounds region in
  start <= pos && pos < end


size :: A.Region -> (Int, Int)
size region =
  let ((sr, sc), (er, ec)) = bounds region in
  (er - sr, if er == sr then ec - sc else ec)


regionToJson :: A.Region -> E.Value
regionToJson region =
  let ((sr, sc), (er, ec)) = bounds region in
  E.object [ "line" ==> E.int sr, "column" ==> E.int sc, "endLine" ==> E.int er, "endColumn" ==> E.int ec ]


-- The source of a region, on one line.
excerpt :: String -> A.Region -> String
excerpt source region =
  let
    ((sr, sc), (er, ec)) = bounds region
    ls = take (er - sr + 1) (drop (sr - 1) (lines source))
    cut =
      case ls of
        []  -> []
        [l] -> [take (ec - sc) (drop (sc - 1) l)]
        l : rest -> drop (sc - 1) l : init rest ++ [take (ec - 1) (last rest)]
  in
  unwords (words (unwords cut))



-- SCOPE
--
-- The local names in scope at a position, innermost last, each with the
-- region where it is bound. Top level definitions are left out.


type Scope = [(N.Name, A.Region)]


scopeAt :: Pos -> Can.Decls -> Scope
scopeAt pos decls =
  case decls of
    Can.Declare def rest         -> Maybe.fromMaybe (scopeAt pos rest) (inDef pos [] def)
    Can.DeclareRec def defs rest -> Maybe.fromMaybe (scopeAt pos rest) (Maybe.listToMaybe (Maybe.mapMaybe (inDef pos []) (def:defs)))
    Can.SaveTheEnvironment       -> []


-- The scope at a position inside a definition's arguments or body.
inDef :: Pos -> Scope -> Can.Def -> Maybe Scope
inDef pos scope def =
  let
    (args, body) = defParts def
    bound = concatMap bindings args
  in
  if inside pos (A.toRegion body)
    then Just (inExpr pos (scope ++ bound) body)
    else if any (inside pos . A.toRegion) args then Just scope
    else Nothing


defParts :: Can.Def -> ([Can.Pattern], Can.Expr)
defParts def =
  case def of
    Can.Def _ args body              -> (args, body)
    Can.TypedDef _ _ typedArgs body _ -> (map fst typedArgs, body)


defName :: Can.Def -> (N.Name, A.Region)
defName def =
  case def of
    Can.Def (A.At r n) _ _          -> (n, r)
    Can.TypedDef (A.At r n) _ _ _ _ -> (n, r)


-- Given that the position is inside the expression, the scope at the
-- innermost expression around it.
inExpr :: Pos -> Scope -> Can.Expr -> Scope
inExpr pos scope (A.At _ e) =
  let
    descend children =
      case [ (extra, child) | (extra, child) <- children, inside pos (A.toRegion child) ] of
        (extra, child) : _ -> inExpr pos (scope ++ extra) child
        []                 -> scope

    plain = map ((,) [])
  in
  case e of
    Can.List es                -> descend (plain es)
    Can.Negate a               -> descend (plain [a])
    Can.Widen a                -> descend (plain [a])
    Can.Binop _ _ _ _ a b      -> descend (plain [a, b])
    Can.Lambda args body       -> descend [(concatMap bindings args, body)]
    Can.Call f args            -> descend (plain (f : args))
    Can.If branches final      -> descend (plain (concatMap (\(c, b) -> [c, b]) branches ++ [final]))
    Can.Let def body           ->
      let name = defName def in
      Maybe.fromMaybe (descend [([name], body)]) (inDef pos (scope ++ [name]) def)
    Can.LetRec defs body       ->
      let names = map defName defs in
      Maybe.fromMaybe (descend [(names, body)]) (Maybe.listToMaybe (Maybe.mapMaybe (inDef pos (scope ++ names)) defs))
    Can.LetDestruct p a body   -> descend [([], a), (bindings p, body)]
    Can.Case subject branches  -> descend (([], subject) : [ (bindings p, b) | Can.CaseBranch p b <- branches ])
    Can.Access a _             -> descend (plain [a])
    Can.Update _ a fields      -> descend (plain (a : [ f | Can.FieldUpdate _ f <- Map.elems fields ]))
    Can.Record fields          -> descend (plain (Map.elems fields))
    Can.Pair a b               -> descend (plain [a, b])
    Can.Triple a b c           -> descend (plain [a, b, c])
    _                          -> scope


-- The names a pattern binds, with the regions the type checker records
-- them under.
bindings :: Can.Pattern -> Scope
bindings (A.At region p) =
  case p of
    Can.PVar name           -> [(name, region)]
    Can.PAlias inner name   -> bindings inner ++ [(name, region)]
    Can.PRecord fields      -> [ (field, region) | field <- fields ]
    Can.PPair a b           -> bindings a ++ bindings b
    Can.PTriple a b c       -> bindings a ++ bindings b ++ bindings c
    Can.PList ps            -> concatMap bindings ps
    Can.PCons a b           -> bindings a ++ bindings b
    Can.PCtor _ _ _ _ _ args -> concatMap (bindings . Can._arg) args
    Can.PTag _ _ _ args     -> concatMap bindings args
    _                       -> []


bindingType :: [Solve.Probed] -> N.Name -> A.Region -> Maybe Can.Annotation
bindingType probes name region =
  Maybe.listToMaybe
    [ ann | Solve.Probed r (Just n) ann <- probes, n == name, bounds r == bounds region ]

