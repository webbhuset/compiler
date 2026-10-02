module Tool.Project
  ( withModule
  , withProject
  , parseModuleName
  , parseValueName
  )
  where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified String as S
import qualified ThreadSafe.Fork as Fork

import qualified AST.Prim.Module as ModuleName
import qualified AST.Prim.Name as N
import qualified Build
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as Canonical
import qualified Elm.Version as V
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified File
import qualified Json.Decode as JD
import qualified Reporting
import qualified Reporting.Error as Error
import qualified Reporting.Exit as Exit
import qualified Root as R
import Tool.Problem (Problem(..))
import qualified Tool.Module as Module
import qualified Tool.Interface as Interface
import qualified Tool.Package as Package
import Tool.Summary (Summary)



-- LOAD


-- Summarize a module, type checking it if it belongs to the project, and
-- reading the docs.json of its package if it comes from a dependency.
withModule :: Bool -> ModuleName.Name -> (Summary -> IO (Either Problem a)) -> IO (Either Problem a)
withModule everything name callback =
  do  maybeRoot <- R.findRoot
      case maybeRoot of
        Nothing ->
          return (Left NoOutline)

        Just root ->
          R.withRootLock root $ \writer stuff ->
            do  eitherDetails <- Details.load writer Reporting.silent root stuff
                case eitherDetails of
                  Left problem ->
                    return (Left (BadDetails problem))

                  Right details ->
                    do  found <- findModule root details name
                        case found of
                          Left problem ->
                            return (Left problem)

                          Right (InProject path) ->
                            do  source <- File.readUtf8 path
                                result <- Build.fromModule writer root stuff details source
                                case result of
                                  Right checked ->
                                    callback =<< Module.summarize everything (relative root path) checked

                                  Left (Exit.ReplBadInput _ _ err) ->
                                    do  time <- File.getTime path
                                        return $ Left $ BadModule root $
                                          Error.Module name path time source err

                                  Left problem ->
                                    return (Left (BadBuild problem))

                          Right (InPackage pkg) ->
                            do  result <- loadPackage root stuff details pkg name
                                case result of
                                  Left problem -> return (Left problem)
                                  Right summary -> callback summary


-- Type check every module in the source directories.
withProject :: (FilePath -> [(FilePath, Build.Checked)] -> IO (Either Problem a)) -> IO (Either Problem a)
withProject callback =
  do  maybeRoot <- R.findRoot
      case maybeRoot of
        Nothing ->
          return (Left NoOutline)

        Just root ->
          R.withRootLock root $ \writer stuff ->
            do  eitherDetails <- Details.load writer Reporting.silent root stuff
                case eitherDetails of
                  Left problem ->
                    return (Left (BadDetails problem))

                  Right details ->
                    do  names <- concat <$> traverse findModules (sourceDirs root details)
                        result <- Build.fromProject writer root stuff details names
                        case result of
                          Left problem ->
                            return (Left (BadBuild problem))

                          Right modules ->
                            callback (R.toAbsolutePath root (R.Relative "")) [ (relative root path, c) | (path, c) <- modules ]


sourceDirs :: R.Root -> Details.Details -> [FilePath]
sourceDirs root (Details.Details _ outline _ _ _ _) =
  case outline of
    Details.ValidApp dirs  -> map (R.toAbsolutePath root) (NE.toList dirs)
    Details.ValidPkg _ _ _ -> [R.toAbsolutePath root (R.Relative "src")]


-- The modules under a source directory, named by their paths.
findModules :: FilePath -> IO [ModuleName.Name]
findModules dir =
  go []
  where
    go segments =
      do  let here = foldl (</>) dir segments
          exists <- Dir.doesDirectoryExist here
          entries <- if exists then Dir.listDirectory here else return []
          found <- traverse (visit segments here) (List.sort entries)
          return (concat found)

    visit segments here entry =
      do  isDir <- Dir.doesDirectoryExist (here </> entry)
          if isDir
            then if isUpperSegment entry then go (segments ++ [entry]) else return []
            else
              case FP.splitExtension entry of
                (base, ".elm") | isUpperSegment base ->
                  return [ModuleName.fromString (S.fromChars (List.intercalate "." (segments ++ [base])))]
                _ ->
                  return []


relative :: R.Root -> FilePath -> FilePath
relative root path =
  FP.makeRelative (R.toAbsolutePath root (R.Relative "")) path


data Found
  = InProject FilePath
  | InPackage Pkg.Name


findModule :: R.Root -> Details.Details -> ModuleName.Name -> IO (Either Problem Found)
findModule root details@(Details.Details _ _ _ _ foreigns _) name =
  let
    srcDirs = sourceDirs root details

    candidates =
      map (\dir -> dir </> ModuleName.toFilePath name <.> "elm") srcDirs
  in
  do  found <- filterM' Dir.doesFileExist candidates
      case (found, Map.lookup name foreigns) of
        (path:_, _) ->
          return (Right (InProject path))

        ([], Just (Details.Foreign pkg _)) ->
          return (Right (InPackage pkg))

        ([], Nothing) ->
          return (Left (ModuleNotFound name srcDirs))



-- PACKAGES


loadPackage :: R.Root -> R.Stuff -> Details.Details -> Pkg.Name -> ModuleName.Name -> IO (Either Problem Summary)
loadPackage root stuff details@(Details.Details _ validOutline _ _ _ _) pkg name =
  do  versions <-
        case validOutline of
          Details.ValidPkg _ _ vs ->
            return vs

          Details.ValidApp _ ->
            do  result <- Outline.read root
                return $
                  case result of
                    Right (Outline.App (Outline.AppOutline _ _ direct indirect testDirect testIndirect _)) ->
                      Map.unions [direct, indirect, testDirect, testIndirect]
                    _ ->
                      Map.empty

      let location = Pkg.toChars pkg ++ maybe "" ((" " ++) . V.toChars) (Map.lookup pkg versions)
      fromDocs <- traverse (loadDocs pkg name) (Map.lookup pkg versions)
      case fromDocs of
        Just (Just summary) ->
          return (Right summary)

        _ ->
          do  interfaces <- Fork.await =<< Details.loadInterfaces stuff details
              return $
                case Map.lookup (Canonical.Canonical pkg name) =<< interfaces of
                  Just (I.Public iface) -> Right (Interface.summarize location name iface)
                  _                     -> Left (ModuleInPackage name (Pkg.toChars pkg))


loadDocs :: Pkg.Name -> ModuleName.Name -> V.Version -> IO (Maybe Summary)
loadDocs pkg name vsn =
  do  cache <- R.getPackageCache
      let path = R.package cache pkg vsn </> "docs.json"
      exists <- File.exists path
      if not exists
        then return Nothing
        else
          do  bytes <- File.readUtf8 path
              result <- JD.fromByteString Package.decoder bytes
              return $
                case result of
                  Right docs -> Package.summarize pkg vsn name <$> Map.lookup name docs
                  Left _     -> Nothing


filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' p xs =
  map fst . filter snd . zip xs <$> traverse p xs



-- NAMES


-- `Some.Module`
parseModuleName :: String -> Maybe ModuleName.Name
parseModuleName string =
  if all isUpperSegment (splitOn '.' string)
    then Just (ModuleName.fromString (S.fromChars string))
    else Nothing


-- `Some.Module.value`
parseValueName :: String -> Maybe (ModuleName.Name, N.Name)
parseValueName string =
  case unsnoc (splitOn '.' string) of
    Just (home@(_:_), value) | all isUpperSegment home && isLowerSegment value ->
      Just
        ( ModuleName.fromString (S.fromChars (List.intercalate "." home))
        , N.fromString (S.fromChars value)
        )

    _ ->
      Nothing


isUpperSegment :: String -> Bool
isUpperSegment segment =
  case segment of
    c:cs -> Char.isUpper c && all isInner cs
    []   -> False


isLowerSegment :: String -> Bool
isLowerSegment segment =
  case segment of
    c:cs -> Char.isLower c && all isInner cs
    []   -> False


isInner :: Char -> Bool
isInner c =
  Char.isAlphaNum c || c == '_'


splitOn :: Char -> String -> [String]
splitOn sep string =
  case break (== sep) string of
    (chunk, [])     -> [chunk]
    (chunk, _:rest) -> chunk : splitOn sep rest


unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs =
  case reverse xs of
    []     -> Nothing
    x:rest -> Just (reverse rest, x)
