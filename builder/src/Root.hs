{-# LANGUAGE EmptyDataDecls #-}
module Root
  ( Root
  , pwd
  , findRoot
  --
  , Stuff
  , getStuff
  --
  , elm_json
  , src
  , readme
  , license
  --
  , details
  , interfaces
  , objects
  , elmi
  , elmo
  --
  , Path(..)
  , toAbsolutePath
  , toRelativePath
  , toOriginalPath
  --
  , PROJECT
  , withRootLock
  , PACKAGES
  , withRegistryLock
  --
  , PackageCache
  , getPackageCache
  , registry
  , package
  , packageRoot
  , getPackageArtifacts
  , clearDevCaches
  , readBuildId
  --
  , getReplCache
  , getReplTmpRoot
  --
  , prepublishDir
  --
  , ElmHome(..)
  , getElmHome
  )
  where


import qualified Control.Exception as E
import Control.Monad (filterM)
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as List
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as Time
import qualified System.IO.Unsafe as Unsafe
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FileLock as Lock
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified BuildCount
import qualified File

import qualified AST.Prim.Module as Module
import qualified Elm.Package as Pkg
import qualified Elm.Version as V



-- ROOT


newtype Root = Root FilePath


pwd :: IO Root
pwd =
  Root <$> Dir.getCurrentDirectory


findRoot :: IO (Maybe Root)
findRoot =
  do  dir <- Dir.getCurrentDirectory
      findRootHelp (FP.splitDirectories dir)


findRootHelp :: [String] -> IO (Maybe Root)
findRootHelp dirs =
  case dirs of
    [] ->
      return Nothing

    _:_ ->
      do  exists <- Dir.doesFileExist (FP.joinPath dirs </> "elm.json")
          if exists
            then return $ Just $ Root $ FP.joinPath dirs
            else findRootHelp (init dirs)



-- STUFF


newtype Stuff = Stuff FilePath


getStuff :: Root -> IO Stuff
getStuff (Root root) =
  do  let stuffDir = root </> "elm-stuff"
      let dir = stuffDir </> cacheName stuffCache
      Dir.createDirectoryIfMissing True dir
      tidyDevCaches stuffCache stuffDir
      writeOrigin dir
      return (Stuff dir)


stuffCache :: CacheKind
stuffCache =
  CacheKind (cacheVersion ++ "-") ""


-- A note naming the build that created a cache folder, for whoever looks
-- inside elm-stuff to decide what to delete; the folder name alone is a
-- commit hash or a dev build's binary. Only a label: nothing reads it back.
-- It runs before the project lock, so two commands (or two reactor requests)
-- can race to write it; a failed write is ignored rather than failing them.
writeOrigin :: FilePath -> IO ()
writeOrigin dir =
  E.handle ignoreIOError $
  do  let path = dir </> "origin.txt"
      exists <- Dir.doesFileExist path
      if exists
        then return ()
        else writeFile path $
          "Created by Webbhuset Elm " ++ V.toChars V.compiler
          ++ " (" ++ BuildCount.description ++ ")\n"


ignoreIOError :: E.IOException -> IO ()
ignoreIOError =
  ignoringIOError ()


ignoringIOError :: a -> E.IOException -> IO a
ignoringIOError fallback _ =
  return fallback



-- PATHS


elm_json :: Root -> FilePath
src      :: Root -> FilePath
readme   :: Root -> FilePath
license  :: Root -> FilePath

elm_json (Root root) = root </> "elm.json"
src      (Root root) = root </> "src"
readme   (Root root) = root </> "README.md"
license  (Root root) = root </> "LICENSE"


details    :: Stuff -> FilePath
interfaces :: Stuff -> FilePath
objects    :: Stuff -> FilePath

details    (Stuff stuff) = stuff </> "d.dat"
interfaces (Stuff stuff) = stuff </> "i.dat"
objects    (Stuff stuff) = stuff </> "o.dat"


elmi :: Stuff -> Module.Name -> FilePath
elmo :: Stuff -> Module.Name -> FilePath

elmi (Stuff stuff) name = stuff </> Module.toDashPath name <.> "elmi"
elmo (Stuff stuff) name = stuff </> Module.toDashPath name <.> "elmo"



-- PATH


data Path
  = Absolute FilePath
  | Relative FilePath
  deriving (Eq)


toAbsolutePath :: Root -> Path -> FilePath
toAbsolutePath (Root root) path =
  case path of
    Absolute dir -> dir
    Relative dir -> root </> dir


toRelativePath :: Root -> FilePath -> FilePath
toRelativePath (Root root) path =
  FP.makeRelative root path


toOriginalPath :: Path -> FilePath
toOriginalPath path =
  case path of
    Absolute dir -> dir
    Relative dir -> dir



-- LOCK


data PROJECT


withRootLock :: Root -> (File.Writer t -> Stuff -> IO a) -> IO a
withRootLock root callback =
  File.withWriter $ \writer ->
    do  stuff@(Stuff dir) <- getStuff root
        Lock.withFileLock (dir </> "lock") Lock.Exclusive (\_ -> callback writer stuff)



-- REGISTRY LOCK
--
-- PERF since many artifacts are guaranteed to be the same every time, maybe try
-- to find a way to write temporary files and then move them into place. If two
-- processes overwrite each other that should be okay.


data PACKAGES


withRegistryLock :: PackageCache -> (File.Writer PACKAGES -> IO a) -> IO a
withRegistryLock (PackageCache dir) callback =
  File.withWriter $ \writer ->
    Lock.withFileLock (dir </> "lock") Lock.Exclusive (\_ -> callback writer)



-- PACKAGE CACHES


newtype PackageCache = PackageCache FilePath


getPackageCache :: IO PackageCache
getPackageCache =
  PackageCache <$> getCacheDir "packages"


registry :: PackageCache -> FilePath
registry (PackageCache dir) =
  dir </> "registry.dat"


package :: PackageCache -> Pkg.Name -> V.Version -> FilePath
package (PackageCache dir) name version =
  dir </> Pkg.toFilePath name </> V.toChars version


packageRoot :: PackageCache -> Pkg.Name -> V.Version -> Root
packageRoot c p v =
  Root (package c p v)


-- Named by build, like elm-stuff: see BuildId.
getPackageArtifacts :: PackageCache -> Pkg.Name -> V.Version -> IO FilePath
getPackageArtifacts c p v =
  do  let dir = package c p v
      tidyDevCaches artifactsCache dir
      return (dir </> cacheName artifactsCache)


artifactsCache :: CacheKind
artifactsCache =
  CacheKind "artifacts-" ".dat"



-- CACHE


getReplCache :: IO FilePath
getReplCache =
  getCacheDir "repl"


-- The repl's scratch project. Its compiled output goes to the build-named
-- elm-stuff under it (see BuildId), so the folder itself is shared
-- by every build and the repl history in it survives upgrades.
getReplTmpRoot :: IO Root
getReplTmpRoot =
  do  dir <- getCacheDir "repl"
      return $ Root (dir </> "tmp")


-- The directory name this fork caches under in ELM_HOME. It is prefixed so
-- it can never collide with the official compiler's caches: the two write
-- incompatible interface and object files, so sharing a directory makes them
-- invalidate each other's artifacts on every switch.
--
-- Package sources, the registry, git clones, the repl folder and each
-- package's docs.json live under it and are shared by every build of this
-- compiler version. docs.json is only read by elm diff, elm bump and the
-- reactor, never by code generation. Compiled output is not shared: see
-- BuildId.
cacheVersion :: FilePath
cacheVersion =
  "webbhuset-" ++ V.toChars V.compiler



-- BUILD ID


-- Names compiled output (a project's elm-stuff directory, each package's
-- artifacts file). Builds of one version can generate different code, and
-- output cached by one of them would otherwise be reused by the next.
--
-- A build from a clean checkout, such as a release, is named after its commit
-- (installers/stamp-build-count.sh decides what is clean). Builds of one
-- commit share caches, and several released compilers can share one ELM_HOME
-- without rebuilding each other's packages.
--
-- Any other build is a dev build: its commit doesn't say what it was built
-- from. It is named after its own binary, so rebuilding the compiler after a
-- change gives a new name and nothing from before the change is reused, and it
-- deletes other dev builds' caches beside its own once unused for an hour (see
-- tidyDevCaches), so they don't pile up. Binaries from Nix all have modification
-- time 1, so the name also has the binary's size and a hash of its path
-- (unique per Nix store entry).
--
-- The name is only as fresh as gen/BuildCount.hs: a bare `cabal build` keeps
-- the last stamp, so it can build a dev binary that still uses a commit name.
data BuildId
  = Clean String
  | Dev String


-- Read once per process, at startup (readBuildId): a running reactor or repl
-- keeps the name it started with after its binary is rebuilt (the old binary
-- then reads as deleted, and its output belongs under its own name anyway).
buildId :: BuildId
buildId =
  Unsafe.unsafePerformIO $
  case BuildCount.cleanCommit of
    Just commit ->
      return (Clean commit)

    Nothing ->
      do  exe <- Env.getExecutablePath
          time <- Dir.getModificationTime exe
          size <- Dir.getFileSize exe
          let seconds = floor (Time.utcTimeToPOSIXSeconds time) :: Integer
          let pathHash = take 8 (SHA.showDigest (SHA.sha1 (LChar8.pack exe)))
          return $ Dev (show seconds ++ "-" ++ show size ++ "-" ++ pathHash)
{-# NOINLINE buildId #-}


-- Called first thing in main. buildId would otherwise be read at the first
-- cache access, which in an idle reactor or repl can come after the binary was
-- rebuilt, when it no longer exists.
readBuildId :: IO ()
readBuildId =
  () <$ E.evaluate buildId


-- A kind of compiled cache, named prefix ++ build ++ suffix.
data CacheKind =
  CacheKind
    { _prefix :: String
    , _suffix :: String
    }


-- This build's cache of the given kind.
cacheName :: CacheKind -> FilePath
cacheName (CacheKind prefix suffix) =
  case buildId of
    Clean commit -> prefix ++ commit ++ suffix
    Dev binary -> prefix ++ devMarker ++ binary ++ suffix


devMarker :: String
devMarker =
  "dev-"


-- For a dev build: marks its own cache of this kind in `dir` as used (its
-- modification time; reading a cache doesn't change it), and deletes other dev
-- builds' caches of this kind that nothing has used for an hour. A project
-- folder is marked at every command, so dev compilers running at the same
-- time in parallel workspaces keep theirs, and so does one being compiled
-- into. A package's artifacts file is only marked when a project sets up its
-- dependencies (they're copied into the project's own folder), so after an
-- hour without that another dev build may delete it, and the next setup
-- compiles the package again. Abandoned dev builds' caches go. Clean builds'
-- caches are never deleted. Best effort throughout: another process may be
-- using or removing the same files.
tidyDevCaches :: CacheKind -> FilePath -> IO ()
tidyDevCaches kind@(CacheKind prefix suffix) dir =
  case buildId of
    Clean _ ->
      return ()

    Dev _ ->
      E.handle ignoreIOError $
      do  now <- Clock.getCurrentTime
          let own = cacheName kind
          E.handle ignoreIOError (Dir.setModificationTime (dir </> own) now)
          entries <- Dir.listDirectory dir
          let isOtherDev entry =
                entry /= own
                && List.isPrefixOf (prefix ++ devMarker) entry
                && List.isSuffixOf suffix entry
          mapM_ (E.handle ignoreIOError . removeIfUnused now . (dir </>)) (filter isOtherDev entries)


removeIfUnused :: Clock.UTCTime -> FilePath -> IO ()
removeIfUnused now path =
  do  used <- Dir.getModificationTime path
      if Clock.diffUTCTime now used > unusedFor
        then Dir.removePathForcibly path
        else return ()


-- How long another dev build's cache must go unused before it is deleted.
unusedFor :: Clock.NominalDiffTime
unusedFor =
  60 * 60


-- The reactor's front end is compiled by Template Haskell, which runs this
-- code inside GHC: getExecutablePath is GHC's binary there, so a dev build's
-- name would survive changes to the compiler. Deleting this name's caches for
-- the reactor's project and its packages before compiling it avoids reusing
-- them.
clearDevCaches :: Root -> IO ()
clearDevCaches (Root root) =
  case buildId of
    Clean _ ->
      return ()

    Dev _ ->
      do  E.handle ignoreIOError $ Dir.removePathForcibly (root </> "elm-stuff" </> cacheName stuffCache)
          PackageCache dir <- getPackageCache
          versions <- subdirectories 3 dir
          mapM_ (E.handle ignoreIOError . Dir.removePathForcibly . (</> cacheName artifactsCache)) versions


-- The directories exactly `depth` levels below `dir`: packages/author/project/version.
-- A directory that can't be listed, such as one another elm process removes
-- meanwhile, counts as empty.
subdirectories :: Int -> FilePath -> IO [FilePath]
subdirectories depth dir =
  if depth <= 0 then
    return [dir]
  else
    do  entries <- E.handle (ignoringIOError []) (Dir.listDirectory dir)
        dirs <- filterM Dir.doesDirectoryExist (map (dir </>) entries)
        concat <$> mapM (subdirectories (depth - 1)) dirs


getCacheDir :: FilePath -> IO FilePath
getCacheDir projectName =
  do  (ElmHome home) <- getElmHome
      let root = home </> cacheVersion </> projectName
      Dir.createDirectoryIfMissing True root
      return root



-- PUBLISH


prepublishDir :: Root -> (Root, FilePath)
prepublishDir (Root root) =
    (Root dir, dir)
  where
    dir = root </> "prepublish"



-- ELM HOME


newtype ElmHome =
  ElmHome FilePath


getElmHome :: IO ElmHome
getElmHome =
  do  maybeCustomHome <- Env.lookupEnv "ELM_HOME"
      case maybeCustomHome of
        Just customHome -> return $ ElmHome customHome
        Nothing -> ElmHome <$> Dir.getAppUserDataDirectory "elm"

