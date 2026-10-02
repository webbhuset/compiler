module Tool.Refs
  ( run
  , parseTarget
  , checkExists
  )
  where


import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Build
import qualified Json.Encode as E
import Tool.Index (Ref(..))
import qualified Tool.Index as Index
import Tool.Output (Output(..))
import qualified Tool.Place as Place
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import qualified Tool.Summary as Summary



-- RUN
--
--   elm tool refs Some.Module.value   where a value is defined and used
--   elm tool refs Some.Module.Name    a type, or a constructor, or both
--
-- One line per place, definitions first, like grep:
--
--   src/Page/Home.elm:42:5: update msg model =
--   src/Main.elm:61:22: Home.update subMsg home


run :: String -> IO (Either Problem Output)
run target =
  case parseTarget target of
    Nothing ->
      return (Left (BadName "a value like `Page.Home.view` or a type like `Page.Home.Model`" target))

    Just (home, name) ->
      Project.withProject $ \root _ modules ->
        case checkExists home name modules of
          Just suggestions ->
            return $ Left $ NotFound ("anything in " ++ Module.toChars home) name suggestions

          Nothing ->
            do  let found =
                      [ (Index._kind r == Index.Definition, Place.fromRegion (Index._path m) (Index.kindToChars (Index._kind r)) (_region r))
                      | m <- Index.index modules
                      , r <- Index._refs m
                      , _target r == (home, name)
                      ]
                    sorted = map snd (List.sortOn (\(isDef, p) -> (not isDef, Place._path p, Place._line p, Place._column p)) found)
                places <- Place.withSource root sorted
                return $ Right $ Output (concatMap Place.toText places) (E.list Place.toJson places)


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


-- Nothing when the name exists, otherwise the names that are close.
checkExists :: Module.Name -> String -> [(FilePath, Build.Checked)] -> Maybe [String]
checkExists home name modules =
  let
    candidates = Maybe.fromMaybe [] (Index.namesIn home modules)
  in
  if elem name candidates then Nothing else Just (Summary.suggest name candidates)
