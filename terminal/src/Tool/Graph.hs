{-# LANGUAGE OverloadedStrings #-}
module Tool.Graph
  ( graph
  , why
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Prim.Module as Module
import qualified Elm.Package as Pkg
import qualified Json.Encode as E
import Json.Encode ((==>))
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import Tool.Project (Graph(..))
import qualified Tool.Project as Project



-- GRAPH
--
--   elm tool graph               every project module and what it imports
--   elm tool graph Some.Module   what one module imports and what imports it
--
-- Project modules come before the `|`, package modules after it.


graph :: [String] -> IO (Either Problem Output)
graph args =
  case args of
    [] ->
      Project.withGraph $ \g ->
        do  edges <- projectEdges g
            return (Right (whole g edges))

    [target] ->
      case Project.parseModuleName target of
        Nothing -> return (Left (BadName "a module like `Page.Home`" target))
        Just name ->
          Project.withGraph $ \g ->
            do  edges <- projectEdges g
                imports <- _importsOf g name
                return $
                  if Map.member name (_locals g) || any (elem name) (Map.elems edges) || not (null imports)
                    then Right (one g edges name imports)
                    else Left (NotFound "a module" target [])

    _ ->
      return (Left (BadArgs "graph" "elm tool graph [Some.Module]"))


projectEdges :: Graph -> IO (Map.Map Module.Name [Module.Name])
projectEdges g =
  Map.fromList <$> traverse (\m -> (,) m <$> _importsOf g m) (Map.keys (_locals g))


whole :: Graph -> Map.Map Module.Name [Module.Name] -> Output
whole g edges =
  let
    isLocal m = Map.member m (_locals g)
    line (m, imports) =
      let (local, foreign) = List.partition isLocal (byName imports) in
      Module.toChars m ++ " -> " ++ commaSep (map Module.toChars local)
      ++ (if null foreign then "" else " | " ++ commaSep (map Module.toChars foreign)) ++ "\n"

    imported = Set.fromList (concat (Map.elems edges))
    roots = byName [ m | m <- Map.keys edges, Set.notMember m imported ]
    chain = longestChain (\m -> filter isLocal (Map.findWithDefault [] m edges)) roots
  in
  Output
    ( concatMap line (List.sortOn (Module.toChars . fst) (Map.toList edges))
      ++ "\n" ++ show (Map.size edges) ++ " modules. Imported by nothing: " ++ commaSep (map Module.toChars roots) ++ ".\n"
      ++ "Longest import chain (" ++ show (length chain) ++ "): " ++ List.intercalate " -> " (map Module.toChars chain) ++ "\n"
    )
    ( E.object
        [ "modules" ==> E.list (\(m, imports) ->
            E.object
              [ "module" ==> E.chars (Module.toChars m)
              , "path" ==> E.chars (Map.findWithDefault "" m (_locals g))
              , "imports" ==> E.list (E.chars . Module.toChars) (filter isLocal imports)
              , "packageImports" ==> E.list (E.chars . Module.toChars) (filter (not . isLocal) imports)
              ]) (Map.toList edges)
        , "roots" ==> E.list (E.chars . Module.toChars) roots
        , "longestChain" ==> E.list (E.chars . Module.toChars) chain
        ]
    )


one :: Graph -> Map.Map Module.Name [Module.Name] -> Module.Name -> [Module.Name] -> Output
one g edges name imports =
  let
    isLocal m = Map.member m (_locals g)
    importers = byName [ m | (m, is) <- Map.toList edges, elem name is ]
    localNext m = filter isLocal (Map.findWithDefault [] m edges)
    dependsOn = Set.delete name (reach localNext [name])
    dependents = Set.delete name (reach (\m -> [ x | (x, is) <- Map.toList edges, elem m is ]) [name])
    (local, foreign) = List.partition isLocal (byName imports)
    origin = maybe "" (\p -> " (" ++ Pkg.toChars p ++ ")") (_packageOf g name)
  in
  Output
    ( Module.toChars name ++ origin ++ "\n"
      ++ "imports: " ++ commaSep (map Module.toChars local) ++ (if null foreign then "" else " | " ++ commaSep (map Module.toChars foreign)) ++ "\n"
      ++ "imported by: " ++ commaSep (map Module.toChars importers) ++ "\n"
      ++ "depends on " ++ show (Set.size dependsOn) ++ " project modules, and " ++ show (Set.size dependents) ++ " project modules depend on it.\n"
    )
    ( E.object
        [ "module" ==> E.chars (Module.toChars name)
        , "imports" ==> E.list (E.chars . Module.toChars) (List.sort imports)
        , "importedBy" ==> E.list (E.chars . Module.toChars) importers
        , "dependsOn" ==> E.list (E.chars . Module.toChars) (Set.toList dependsOn)
        , "dependents" ==> E.list (E.chars . Module.toChars) (Set.toList dependents)
        ]
    )


reach :: (Module.Name -> [Module.Name]) -> [Module.Name] -> Set.Set Module.Name
reach next =
  go Set.empty
  where
    go seen todo =
      case todo of
        [] -> seen
        m : rest
          | Set.member m seen -> go seen rest
          | otherwise -> go (Set.insert m seen) (next m ++ rest)


-- Elm has no import cycles, so the longest path is well defined.
longestChain :: (Module.Name -> [Module.Name]) -> [Module.Name] -> [Module.Name]
longestChain next roots =
  let
    nodes = Set.toList (reach next roots)
    memo = Map.fromList [ (m, chainFrom m) | m <- nodes ]
    chainFrom m =
      m : List.foldl' (\best x -> let c = Map.findWithDefault [] x memo in if length c > length best then c else best) [] (next m)
  in
  List.foldl' (\best r -> let c = Map.findWithDefault [] r memo in if length c > length best then c else best) [] roots


byName :: [Module.Name] -> [Module.Name]
byName =
  List.sortOn Module.toChars


commaSep :: [String] -> String
commaSep =
  List.intercalate ", "



-- WHY
--
--   elm tool why Some.Module
--
-- The shortest chain of imports from each module with a `main` to a module,
-- which may be a package module:
--
--   Main -> Page.Settings -> Json.Decode


why :: String -> IO (Either Problem Output)
why target =
  case Project.parseModuleName target of
    Nothing ->
      return (Left (BadName "a module like `Json.Decode`" target))

    Just name ->
      Project.withGraph $ \g ->
        do  let starts = if null (_mains g) then Map.keys (_locals g) else _mains g
            chains <- traverse (\s -> (,) s <$> shortest g s name) starts
            let found = [ c | (_, Just c) <- chains ]
            return $
              if null found
                then Right (Output ("Nothing with a `main` imports " ++ target ++ ", directly or not.\n") (E.list id []))
                else Right $ Output
                  (concatMap (\c -> List.intercalate " -> " (map Module.toChars c) ++ "\n") found)
                  (E.list (E.list (E.chars . Module.toChars)) found)


-- Breadth first, so the first chain found is a shortest one.
shortest :: Graph -> Module.Name -> Module.Name -> IO (Maybe [Module.Name])
shortest g from to =
  go (Set.singleton from) [(from, [from])]
  where
    -- each module reached, with the chain to it, newest first
    go seen frontier =
      case [ path | (m, path) <- frontier, m == to ] of
        path : _ ->
          return (Just (reverse path))

        [] | null frontier ->
          return Nothing

        [] ->
          do  nexts <- traverse (\(m, path) -> map (\x -> (x, x : path)) <$> _importsOf g m) frontier
              let fresh = Map.toList (Map.fromListWith (\_ first -> first) [ step | step@(m, _) <- concat nexts, Set.notMember m seen ])
              go (Set.union seen (Set.fromList (map fst fresh))) fresh
