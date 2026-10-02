{-# LANGUAGE OverloadedStrings #-}
module Tool.Hole
  ( run
  )
  where


import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.List as List
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Build
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import qualified Type.Solve as Solve
import qualified Tool.At as At
import qualified Tool.Index as Index
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import qualified Tool.Render as Render
import qualified Tool.Unify as Unify



-- RUN
--
--   elm tool hole src/Page/Home.elm:42:17
--
-- Put `Debug.todo "?"`, or anything else that type checks, where something
-- is missing, and ask what goes there: the type the place needs, the names
-- in scope that have it, and the ones that get there with more arguments.
--
--   Debug.todo "?" : Html Msg
--
--   Fits:
--       viewHeader : Html Msg
--
--   With 1 more argument:
--       Html.text : String -> Html msg


run :: String -> IO (Either Problem Output)
run target =
  case At.parseTarget target of
    Nothing ->
      return (Left (BadName "a position like `src/Main.elm:42:17`" target))

    Just (path, pos) ->
      Project.withProbe path $ \source (Build.Probed src can annotations probes ifaces) ->
        let
          text = BS_UTF8.toString source
          around = List.sortOn (\(Solve.Probed r _ _) -> At.size r) [ p | p@(Solve.Probed r _ _) <- probes, At.inside pos r ]
          ns = Render.names src
          render ann = Render.oneLine (Render.annotation ns (Render.normalize ann))
        in
        case hole text around of
          Nothing ->
            return (Left (NothingAt target))

          Just (Solve.Probed region _ expected@(Can.Forall _ expectedType)) ->
            let
              scope = At.scopeAt pos (Can._decls can)
              locals = [ (N.toChars n, ann) | (n, r) <- scope, Just ann <- [At.bindingType probes n r] ]
              declared = declaredTypes (Can._decls can)
              tops = [ (N.toChars n, Map.findWithDefault ann n declared) | (n, ann) <- Map.toList annotations, isWritten src n ] ++ localCtors can
              imported = importedValues src ifaces
              candidates = dedupe (reverse locals ++ tops ++ imported)

              matches =
                [ (k, name, ann)
                | (name, ann@(Can.Forall _ t)) <- candidates
                , Just k <- [Unify.fits t expectedType]
                ]

              group k = [ (name, ann) | (k', name, ann) <- matches, k' == k ]
              groups = [ (k, group k) | k <- List.nub (List.sort [ k | (k, _, _) <- matches ]) ]

              line (name, ann) = "    " ++ name ++ " : " ++ render ann ++ "\n"
              heading k = if k == 0 then "Fits:" else "With " ++ show k ++ " more argument" ++ (if k == 1 then "" else "s") ++ ":"
            in
            return $ Right $ Output
              ( At.excerpt text region ++ " : " ++ render expected ++ "\n"
                ++ concat [ "\n" ++ heading k ++ "\n" ++ concatMap line (take 15 es) ++ more es | (k, es) <- groups ]
                ++ (if null groups then "\nNothing in scope fits.\n" else "")
              )
              ( E.object
                  [ "expression" ==> E.chars (At.excerpt text region)
                  , "type" ==> E.chars (render expected)
                  , "fits" ==> E.list (\(k, name, ann) -> E.object [ "name" ==> E.chars name, "type" ==> E.chars (render ann), "arguments" ==> E.int k ]) matches
                  ]
              )


more :: [a] -> String
more es =
  if length es > 15 then "    ... and " ++ show (length es - 15) ++ " more\n" else ""


-- The expression standing in for what is missing: the innermost one at the
-- position, or the call around it when that is the `Debug.todo` itself.
hole :: String -> [Solve.Probed] -> Maybe Solve.Probed
hole text around =
  case around of
    p@(Solve.Probed r _ _) : rest
      | At.excerpt text r `elem` ["Debug.todo", "todo"] ->
          case [ q | q@(Solve.Probed r' _ _) <- rest, At.size r' > At.size r ] of
            outer : _ -> Just outer
            []        -> Just p
      | otherwise ->
          Just p

    [] ->
      Nothing



-- CANDIDATES


isWritten :: Src.Module -> N.Name -> Bool
isWritten src n =
  or [ v == n | A.At _ (Src.Value (A.At _ v) _ _ _) <- Src._values src ]


-- The annotations as written, which keep their aliases.
declaredTypes :: Can.Decls -> Map.Map N.Name Can.Annotation
declaredTypes decls =
  let
    typed def =
      case def of
        Can.TypedDef (A.At _ n) free args _ result -> [(n, Can.Forall free (foldr (Can.TLambda . snd) result args))]
        Can.Def _ _ _ -> []
  in
  case decls of
    Can.Declare def rest         -> Map.union (Map.fromList (typed def)) (declaredTypes rest)
    Can.DeclareRec def defs rest -> Map.union (Map.fromList (concatMap typed (def : defs))) (declaredTypes rest)
    Can.SaveTheEnvironment       -> Map.empty


localCtors :: Can.Module -> [(String, Can.Annotation)]
localCtors can =
  concat [ ctorsOf (Can._name can) t u | (t, u) <- Map.toList (Can._unions can) ]


ctorsOf :: ModuleName.Canonical -> T.Name -> Can.Union -> [(String, Can.Annotation)]
ctorsOf home typeName (Can.Union vars ctors _ _) =
  let
    result = Can.TType home typeName (map Can.TVar vars)
    free = Map.fromList [ (v, ()) | v <- vars ]
  in
  [ (N.toChars c, Can.Forall free (foldr Can.TLambda result args)) | Can.Ctor c _ _ args <- ctors ]


-- What each import brings in, named the way this module has to write it.
importedValues :: Src.Module -> Map.Map Module.Name I.Interface -> [(String, Can.Annotation)]
importedValues src ifaces =
  concat
    [ [ (written exposing qualifier (N.toChars n), ann) | (n, ann) <- Map.toList (I._values iface) ]
      ++ [ (written exposing qualifier c, ann)
         | (t, iu) <- Map.toList (I._unions iface)
         , Just u <- [Index.publicUnion iu]
         , (c, ann) <- ctorsOf (ModuleName.Canonical (I._home iface) m) t u
         ]
    | Src.Import (A.At _ m) alias exposing _ <- Src._imports src
    , Just iface <- [Map.lookup m ifaces]
    , let qualifier = maybe (Module.toChars m) Module.prefixToChars alias
    ]


written :: Src.Exposing -> String -> String -> String
written exposing qualifier name =
  case exposing of
    Src.Open -> name
    Src.Explicit exposed
      | or [ N.toChars n == name | Src.Lower (A.At _ n) <- exposed ] -> name
      | otherwise -> qualifier ++ "." ++ name


dedupe :: [(String, a)] -> [(String, a)]
dedupe =
  List.nubBy (\a b -> fst a == fst b)
