{-# LANGUAGE BangPatterns #-}
module Generate
  ( Format(..)
  , Bundles(..)
  , WorkerBundle(..)
  , finalize
  , finalizeWith
  , debug
  , dev
  , prod
  , repl
  )
  where


import Prelude hiding (cycle, print)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Monad (liftM2)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as N
import qualified Data.NonEmptyList as NE

import qualified AST.Optimized as Opt
import qualified Build
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Generate.Css as GenCss
import qualified Generate.JavaScript as JS
import qualified Generate.Mode as Mode
import qualified Generate.Workers as Workers
import qualified Nitpick.Debug as Nitpick
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff


-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?



-- GENERATORS


type Task a =
  Task.Task Exit.Generate a


data Format
  = Iife
  | Esm


-- The compiled program: the main bundle, its stylesheet, and one bundle
-- per spawned worker program. Worker bundles are in dependency order and
-- contain placeholder tokens where worker file names go; `finalize` turns
-- everything into writable bytes.
data Bundles =
  Bundles
    { _mainJs :: B.Builder
    , _css :: Maybe B.Builder
    , _workerBundles :: [WorkerBundle]
    , _isScript :: Bool
    }


data WorkerBundle =
  WorkerBundle
    { _workerGlobal :: Opt.Global
    , _workerJs :: B.Builder
    }


generateWith :: Format -> Mode.Mode -> Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> [Opt.Global] -> (B.Builder, Maybe B.Builder)
generateWith format =
  case format of
    Iife -> JS.generate
    Esm  -> JS.generateEsm


toBundles :: Format -> Mode.Mode -> Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> Task Bundles
toBundles format mode graph mains =
  case Workers.plan graph mains of
    Left cycleNames ->
      Task.throw (Exit.GenerateWorkerCycle cycleNames)

    Right workerRoots ->
      let
        workers = map (\g -> WorkerBundle g (JS.generateWorkerBundle mode graph g)) workerRoots
      in
      case workerHome mains of
        -- A worker program compiled as the root: the main bundle IS a worker
        -- bundle, with any workers it spawns in turn alongside. It has no page
        -- to style and nothing to export, and a worker can only be a module.
        Just home ->
          if Map.size mains > 1 then
            Task.throw Exit.GenerateWorkerNeedsOneMain
          else
            case format of
              Iife -> Task.throw Exit.GenerateWorkersRequireEsm
              Esm  -> return (Bundles (JS.generateWorkerBundle mode graph (Opt.Global home N._main)) Nothing workers False)

        Nothing ->
          let
            (js, css) = generateWith format mode graph mains workerRoots
            isScript = JS.hasScriptMain mains
          in
          if isScript && Map.size mains > 1
            then Task.throw Exit.GenerateScriptNeedsOneMain
            else return (Bundles js css workers isScript)


workerHome :: Map.Map ModuleName.Canonical Opt.Main -> Maybe ModuleName.Canonical
workerHome mains =
  case [ home | (home, Opt.Worker) <- Map.toList mains ] of
    home : _ -> Just home
    []       -> Nothing


debug :: Format -> FilePath -> Details.Details -> Build.Artifacts -> Task Bundles
debug format root details (Build.Artifacts pkg ifaces roots modules) =
  do  loading <- loadObjects root details modules
      types   <- loadTypes root ifaces modules
      objects <- finalizeObjects loading
      let mode = Mode.Dev (Just types)
      let graph = objectsToGlobalGraph objects
      let mains = gatherMains pkg objects roots
      toBundles format mode graph mains


dev :: Format -> FilePath -> Details.Details -> Build.Artifacts -> Task Bundles
dev format root details (Build.Artifacts pkg _ roots modules) =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      let mode = Mode.Dev Nothing
      let graph = objectsToGlobalGraph objects
      let mains = gatherMains pkg objects roots
      toBundles format mode graph mains


prod :: Format -> FilePath -> Details.Details -> Build.Artifacts -> Task Bundles
prod format root details (Build.Artifacts pkg _ roots modules) =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      checkForDebugUses objects
      let graph = objectsToGlobalGraph objects
      let mains = gatherMains pkg objects roots
      let mode = Mode.Prod (Mode.ShortNames (Mode.shortenFieldNames graph) (GenCss.shortenNames graph mains))
      toBundles format mode graph mains



-- FINALIZE
--
-- Render worker bundles in dependency order, substituting the file names
-- of the workers each bundle spawns, hashing the result to name its file.
-- Then substitute all the names into the main bundle.


finalize :: String -> Bundles -> ([(FilePath, BS.ByteString)], BS.ByteString, Maybe BS.ByteString)
finalize base (Bundles js css workers _) =
  let
    step (table, files) (WorkerBundle global builder) =
      let
        bytes = substitute table (render builder)
        hash = take 16 (SHA.showDigest (SHA.sha1 (LBS.fromStrict bytes)))
        name = base ++ "." ++ hash ++ ".mjs"
      in
      ( (Workers.token global, BS_UTF8.fromString name) : table
      , (name, bytes) : files
      )

    (finalTable, revFiles) = List.foldl' step ([], []) workers
  in
  ( reverse revFiles
  , substitute finalTable (render js)
  , fmap render css
  )


-- Like finalize, but the caller names the worker files. The reactor serves
-- each worker at its own module's URL rather than as a hashed sibling, so
-- only the main bundle is rendered here; a worker's own request renders it.
-- Left is a worker the caller could not name.
finalizeWith :: (Opt.Global -> Maybe String) -> Bundles -> Either Opt.Global (BS.ByteString, Maybe BS.ByteString)
finalizeWith nameOf (Bundles js css workers _) =
  do  table <- traverse toEntry workers
      return (substitute table (render js), fmap render css)
  where
    toEntry (WorkerBundle global _) =
      case nameOf global of
        Just name -> Right (Workers.token global, BS_UTF8.fromString name)
        Nothing   -> Left global


render :: B.Builder -> BS.ByteString
render builder =
  LBS.toStrict (B.toLazyByteString builder)


substitute :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString -> BS.ByteString
substitute table bytes =
  List.foldl' replaceAll bytes table


replaceAll :: BS.ByteString -> (BS.ByteString, BS.ByteString) -> BS.ByteString
replaceAll haystack (needle, replacement) =
  BS.concat (go haystack)
  where
    go bytes =
      case BS.breakSubstring needle bytes of
        (prefix, rest)
          | BS.null rest -> [prefix]
          | otherwise -> prefix : replacement : go (BS.drop (BS.length needle) rest)


repl :: FilePath -> Details.Details -> Bool -> Build.ReplArtifacts -> N.Name -> Task B.Builder
repl root details ansi (Build.ReplArtifacts home modules localizer annotations) name =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      let graph = objectsToGlobalGraph objects
      return $ JS.generateForRepl ansi localizer graph home name (annotations ! name)



-- CHECK FOR DEBUG


checkForDebugUses :: Objects -> Task ()
checkForDebugUses (Objects _ locals) =
  case Map.keys (Map.filter Nitpick.hasDebugUses locals) of
    []   -> return ()
    m:ms -> Task.throw (Exit.GenerateCannotOptimizeDebugValues m ms)



-- GATHER MAINS


gatherMains :: Pkg.Name -> Objects -> NE.List Build.Root -> Map.Map ModuleName.Canonical Opt.Main
gatherMains pkg (Objects _ locals) roots =
  Map.fromList $ Maybe.mapMaybe (lookupMain pkg locals) (NE.toList roots)


lookupMain :: Pkg.Name -> Map.Map ModuleName.Raw Opt.LocalGraph -> Build.Root -> Maybe (ModuleName.Canonical, Opt.Main)
lookupMain pkg locals root =
  let
    toPair name (Opt.LocalGraph maybeMain _ _) =
      (,) (ModuleName.Canonical pkg name) <$> maybeMain
  in
  case root of
    Build.Inside  name     -> toPair name =<< Map.lookup name locals
    Build.Outside name _ g -> toPair name g



-- LOADING OBJECTS


data LoadingObjects =
  LoadingObjects
    { _foreign_mvar :: MVar (Maybe Opt.GlobalGraph)
    , _local_mvars :: Map.Map ModuleName.Raw (MVar (Maybe Opt.LocalGraph))
    }


loadObjects :: FilePath -> Details.Details -> [Build.Module] -> Task LoadingObjects
loadObjects root details modules =
  Task.io $
  do  mvar <- Details.loadObjects root details
      mvars <- traverse (loadObject root) modules
      return $ LoadingObjects mvar (Map.fromList mvars)


loadObject :: FilePath -> Build.Module -> IO (ModuleName.Raw, MVar (Maybe Opt.LocalGraph))
loadObject root modul =
  case modul of
    Build.Fresh name _ graph ->
      do  mvar <- newMVar (Just graph)
          return (name, mvar)

    Build.Cached name _ _ ->
      do  mvar <- newEmptyMVar
          _ <- forkIO $ putMVar mvar =<< File.readBinary (Stuff.elmo root name)
          return (name, mvar)



-- FINALIZE OBJECTS


data Objects =
  Objects
    { _foreign :: Opt.GlobalGraph
    , _locals :: Map.Map ModuleName.Raw Opt.LocalGraph
    }


finalizeObjects :: LoadingObjects -> Task Objects
finalizeObjects (LoadingObjects mvar mvars) =
  Task.eio id $
  do  result  <- readMVar mvar
      results <- traverse readMVar mvars
      case liftM2 Objects result (sequence results) of
        Just loaded -> return (Right loaded)
        Nothing     -> return (Left Exit.GenerateCannotLoadArtifacts)


objectsToGlobalGraph :: Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
  foldr Opt.addLocalGraph globals locals



-- LOAD TYPES


loadTypes :: FilePath -> Map.Map ModuleName.Canonical I.DependencyInterface -> [Build.Module] -> Task Extract.Types
loadTypes root ifaces modules =
  Task.eio id $
  do  mvars <- traverse (loadTypesHelp root) modules
      let !foreigns = Extract.mergeMany (Map.elems (Map.mapWithKey Extract.fromDependencyInterface ifaces))
      results <- traverse readMVar mvars
      case sequence results of
        Just ts -> return (Right (Extract.merge foreigns (Extract.mergeMany ts)))
        Nothing -> return (Left Exit.GenerateCannotLoadArtifacts)


loadTypesHelp :: FilePath -> Build.Module -> IO (MVar (Maybe Extract.Types))
loadTypesHelp root modul =
  case modul of
    Build.Fresh name iface _ ->
      newMVar (Just (Extract.fromInterface name iface))

    Build.Cached name _ ciMVar ->
      do  cachedInterface <- readMVar ciMVar
          case cachedInterface of
            Build.Unneeded ->
              do  mvar <- newEmptyMVar
                  _ <- forkIO $
                    do  maybeIface <- File.readBinary (Stuff.elmi root name)
                        putMVar mvar (Extract.fromInterface name <$> maybeIface)
                  return mvar

            Build.Loaded iface ->
              newMVar (Just (Extract.fromInterface name iface))

            Build.Corrupted ->
              newMVar Nothing
