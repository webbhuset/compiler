module Tool.Project
  ( withModule
  , withProject
  , withProbe
  , withMain
  , check
  , Graph(..)
  , withGraph
  , parseModuleName
  , parseValueName
  )
  where


import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.IORef as IORef
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
import qualified AST.Optimized as Opt
import qualified Build
import qualified Generate
import qualified Reporting.Task as Task
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
import qualified AST.Source as Src
import qualified Parse.Module as Parse
import qualified Reporting.Annotation as A
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


-- Type check one file, keeping the type of every expression in it.
withProbe :: FilePath -> (BS.ByteString -> Build.Probed -> IO (Either Problem a)) -> IO (Either Problem a)
withProbe path callback =
  do  maybeRoot <- R.findRoot
      exists <- Dir.doesFileExist path
      case maybeRoot of
        Nothing ->
          return (Left NoOutline)

        Just _ | not exists ->
          return (Left (FileNotFound path))

        Just root ->
          R.withRootLock root $ \writer stuff ->
            do  eitherDetails <- Details.load writer Reporting.silent root stuff
                case eitherDetails of
                  Left problem ->
                    return (Left (BadDetails problem))

                  Right details ->
                    do  source <- File.readUtf8 path
                        result <- Build.probeModule writer root stuff details source
                        case result of
                          Right probed ->
                            callback source probed

                          Left (Exit.ReplBadInput _ _ err) ->
                            do  time <- File.getTime path
                                absolute <- Dir.makeAbsolute path
                                return $ Left $ BadModule root $
                                  Error.Module (ModuleName.fromString (S.fromChars path)) absolute time source err

                          Left problem ->
                            return (Left (BadBuild problem))


-- Build a program the way `elm make` would, and hand over the optimized
-- program with the size of each global in it.
withMain :: FilePath -> (Opt.GlobalGraph -> Map.Map Canonical.Canonical Opt.Main -> (Opt.Global -> Int) -> IO (Either Problem a)) -> IO (Either Problem a)
withMain path callback =
  do  maybeRoot <- R.findRoot
      exists <- Dir.doesFileExist path
      case maybeRoot of
        Nothing ->
          return (Left NoOutline)

        Just _ | not exists ->
          return (Left (FileNotFound path))

        Just root ->
          R.withRootLock root $ \writer stuff ->
            do  eitherDetails <- Details.load writer Reporting.silent root stuff
                case eitherDetails of
                  Left problem ->
                    return (Left (BadDetails problem))

                  Right details ->
                    do  built <- Build.fromPaths writer Reporting.silent root stuff details (NE.List path [])
                        case built of
                          Left problem ->
                            return (Left (BadMake (Exit.MakeCannotBuild problem)))

                          Right artifacts ->
                            do  analyzed <- Task.run (Generate.analyze stuff details artifacts)
                                case analyzed of
                                  Left problem ->
                                    return (Left (BadMake (Exit.MakeBadGenerate problem)))

                                  Right (graph, mains, sizeOf) ->
                                    callback graph mains sizeOf


-- Build the given files, or every module in the source directories, the way
-- `elm make --output=/dev/null` would. The number of modules checked.
check :: [FilePath] -> IO (Either Problem Int)
check paths =
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
                    do  files <-
                          case paths of
                            [] -> concat <$> traverse (\dir -> map (\n -> dir </> ModuleName.toFilePath n <.> "elm") <$> findModules dir) (sourceDirs root details)
                            _  -> return paths
                        case files of
                          [] ->
                            return (Right 0)

                          f : fs ->
                            do  built <- Build.fromPaths writer Reporting.silent root stuff details (NE.List f fs)
                                case built of
                                  Left problem -> return (Left (BadMake (Exit.MakeCannotBuild problem)))
                                  Right _      -> return (Right (length files))


-- The import graph, from the sources alone: nothing is type checked. The
-- imports of a package module come from its source in the package cache,
-- parsed the first time they are asked for.
data Graph =
  Graph
    { _locals :: Map.Map ModuleName.Name FilePath
    , _mains :: [ModuleName.Name]
    , _importsOf :: ModuleName.Name -> IO [ModuleName.Name]
    , _packageOf :: ModuleName.Name -> Maybe Pkg.Name
    }


withGraph :: (Graph -> IO (Either Problem a)) -> IO (Either Problem a)
withGraph callback =
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

                  Right details@(Details.Details _ outline _ _ foreigns _) ->
                    do  let dirs = sourceDirs root details
                        found <- traverse (\dir -> map (\n -> (n, dir </> ModuleName.toFilePath n <.> "elm")) <$> findModules dir) dirs
                        let locals = Map.fromList (concat found)
                        versions <- packageVersions root details
                        cache <- R.getPackageCache
                        memo <- IORef.newIORef Map.empty

                        let projectType =
                              case outline of
                                Details.ValidPkg pkg _ _ -> Parse.Package pkg
                                Details.ValidApp _       -> Parse.Application

                            fileOf name =
                              case Map.lookup name locals of
                                Just path -> Just (projectType, path)
                                Nothing ->
                                  do  Details.Foreign pkg _ <- Map.lookup name foreigns
                                      vsn <- Map.lookup pkg versions
                                      Just (Parse.Package pkg, R.package cache pkg vsn </> "src" </> ModuleName.toFilePath name <.> "elm")

                            parse name =
                              do  known <- IORef.readIORef memo
                                  case Map.lookup name known of
                                    Just m -> return m
                                    Nothing ->
                                      do  m <- parseFile (fileOf name)
                                          IORef.modifyIORef memo (Map.insert name m)
                                          return m

                        parsed <- traverse (\name -> (,) name <$> parse name) (Map.keys locals)
                        let mains = [ name | (name, Just m) <- parsed, hasMain m ]
                        callback $ Graph
                          { _locals = Map.map (relative root) locals
                          , _mains = mains
                          , _importsOf = \name -> maybe [] explicitImports <$> parse name
                          , _packageOf = \name -> (\(Details.Foreign pkg _) -> pkg) <$> Map.lookup name foreigns
                          }


parseFile :: Maybe (Parse.ProjectType, FilePath) -> IO (Maybe Src.Module)
parseFile file =
  case file of
    Nothing -> return Nothing
    Just (projectType, path) ->
      do  exists <- Dir.doesFileExist path
          if not exists then return Nothing else
            do  source <- File.readUtf8 path
                either (const Nothing) Just <$> Parse.fromByteString projectType source


-- The imports written in the source, without the ones every module gets.
explicitImports :: Src.Module -> [ModuleName.Name]
explicitImports (Src.Module _ _ _ imports _ _ _ _ _ _ _) =
  [ name | Src.Import (A.At region name) _ _ _ <- imports, not (isZero region) ]


isZero :: A.Region -> Bool
isZero (A.Region s e) =
  A.toEditorRowCol s == A.toEditorRowCol e


hasMain :: Src.Module -> Bool
hasMain (Src.Module _ _ _ _ values _ _ _ _ _ _) =
  or [ N.toChars n == "main" | A.At _ (Src.Value (A.At _ n) _ _ _) <- values ]


packageVersions :: R.Root -> Details.Details -> IO (Map.Map Pkg.Name V.Version)
packageVersions root (Details.Details _ validOutline _ _ _ _) =
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
loadPackage root stuff details pkg name =
  do  versions <- packageVersions root details

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
