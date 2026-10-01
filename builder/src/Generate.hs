{-# LANGUAGE BangPatterns, TemplateHaskell #-}
module Generate
  ( Format(..)
  , Bundles(..)
  , WorkerBundle(..)
  , ChunkBundle(..)
  , finalize
  , finalizeWith
  , debug
  , dev
  , prod
  , repl
  )
  where


import Prelude hiding (cycle, print)
import Control.Concurrent (readMVar)
import Control.Monad (liftM2)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set

import qualified ThreadSafe.Fork as Fork

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
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
import qualified Generate.Chunks as Chunks
import qualified Generate.Workers as Workers
import qualified Nitpick.Debug as Nitpick
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Root as R


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
    , _chunkBundles :: [ChunkBundle]
      -- rendered chunks; only ESM output can host them, so this is empty in
      -- every other format even when the program has async imports
    , _hasChunks :: Bool
    }


data WorkerBundle =
  WorkerBundle
    { _workerGlobal :: Opt.Global
    , _workerJs :: B.Builder
    }


-- One code-splitting chunk: the module an `import async` named, and the ES
-- module it compiled to. In dependency order, like the worker bundles, and
-- carrying placeholder tokens where chunk file names go.
data ChunkBundle =
  ChunkBundle
    { _chunkHome :: ModuleName.Canonical
    , _chunkJs :: B.Builder
    }


generateWith :: Format -> Mode.Mode -> Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> [Opt.Global] -> (B.Builder, Maybe B.Builder)
generateWith format =
  case format of
    Iife -> JS.generate
    Esm  -> JS.generateEsm


toBundles :: Format -> Mode.Mode -> Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> Task Bundles
toBundles format mode graph mains =
  do  -- Several applications in one ES module each get a chunk of their
      -- own, fetched when the application is started; the module itself
      -- keeps what they share. Only ESM output can host the files.
      let splitApps =
            case format of
              Esm  -> Map.size mains > 1
              Iife -> False

      maybePlan <-
        case Chunks.plan (Mode.isDebug mode) splitApps graph mains of
          Left cycleNames -> Task.throw (Exit.GenerateChunkCycle cycleNames)
          Right maybePlan -> return maybePlan

      -- Worker bundles are planned over chunk code too: a `Worker.spawn`
      -- inside an async-imported module still needs its own file.
      workerRoots <-
        case Workers.plan graph mains (chunkRoots maybePlan) of
          Left cycleNames -> Task.throw (Exit.GenerateWorkerCycle cycleNames)
          Right roots -> return roots

      case Chunks.insideWorkers (Mode.isDebug mode) graph workerRoots of
        Just home -> Task.throw (Exit.GenerateChunkInWorker (Chunks.homeToChars home))
        Nothing -> return ()

      let workers = map (\g -> WorkerBundle g (JS.generateWorkerBundle mode graph g)) workerRoots

      case workerHome mains of
        -- A worker program compiled as the root: the main bundle IS a worker
        -- bundle, with any workers it spawns in turn alongside. It has no page
        -- to style and nothing to export, and a worker can only be a module.
        Just home ->
          if Map.size mains > 1 then
            Task.throw Exit.GenerateWorkerNeedsOneMain
          else
            case format of
              Iife -> Task.throw Exit.GenerateWorkerNotAProgram
              Esm  -> return (Bundles (JS.generateWorkerBundle mode graph (Opt.Global home N.main)) Nothing workers False [] False)

        Nothing ->
          let
            isScript = JS.hasScriptMain mains
          in
          if isScript && Map.size mains > 1 then
            Task.throw Exit.GenerateScriptNeedsOneMain
          else
            case (format, maybePlan) of
              (Esm, Just chunkPlan) ->
                let
                  (js, css, chunkJs) =
                    JS.generateEsmWithChunks mode graph mains workerRoots chunkPlan
                in
                return (Bundles js css workers isScript (map (uncurry ChunkBundle) chunkJs) True)

              _ ->
                let
                  (js, css) = generateWith format mode graph mains workerRoots
                in
                return (Bundles js css workers isScript [] (Maybe.isJust maybePlan))


chunkRoots :: Maybe Chunks.Plan -> [Opt.Global]
chunkRoots maybePlan =
  case maybePlan of
    Nothing -> []
    Just (Chunks.Plan chunks _) -> concatMap (Set.toList . Chunks._roots) chunks


workerHome :: Map.Map ModuleName.Canonical Opt.Main -> Maybe ModuleName.Canonical
workerHome mains =
  case [ home | (home, Opt.Worker) <- Map.toList mains ] of
    home : _ -> Just home
    []       -> Nothing


debug :: Format -> R.Stuff -> Details.Details -> Build.Artifacts -> Task Bundles
debug format stuff details (Build.Artifacts pkg ifaces roots modules) =
  do  loading <- loadObjects stuff details modules
      types   <- loadTypes stuff ifaces modules
      objects <- finalizeObjects loading
      let graph = objectsToGlobalGraph objects
      let mode = Mode.Dev (Mode.callees (Opt._g_nodes graph)) (Just types)
      let mains = gatherMains pkg objects roots
      toBundles format mode graph mains


dev :: Format -> R.Stuff -> Details.Details -> Build.Artifacts -> Task Bundles
dev format stuff details (Build.Artifacts pkg _ roots modules) =
  do  objects <- finalizeObjects =<< loadObjects stuff details modules
      let graph = objectsToGlobalGraph objects
      let mode = Mode.Dev (Mode.callees (Opt._g_nodes graph)) Nothing
      let mains = gatherMains pkg objects roots
      toBundles format mode graph mains


prod :: Format -> R.Stuff -> Details.Details -> Build.Artifacts -> Task Bundles
prod format stuff details (Build.Artifacts pkg _ roots modules) =
  do  objects <- finalizeObjects =<< loadObjects stuff details modules
      checkForDebugUses objects
      let graph = objectsToGlobalGraph objects
      let mains = gatherMains pkg objects roots
      let mode = Mode.Prod (Mode.callees (Opt._g_nodes graph)) (Mode.ShortNames (Mode.shortenFieldNames graph) (GenCss.shortenNames graph mains))
      toBundles format mode graph mains



-- FINALIZE
--
-- Render worker bundles in dependency order, substituting the file names
-- of the workers each bundle spawns, hashing the result to name its file.
-- Then substitute all the names into the main bundle.


finalize :: String -> Bundles -> ([(FilePath, BS.ByteString)], BS.ByteString, Maybe BS.ByteString)
finalize base (Bundles js css workers _ chunks _) =
  let
    -- A worker's name is resolved with `new URL(name, import.meta.url)`,
    -- which takes a bare name. A chunk's is an `import()` specifier, where
    -- a bare name would mean a package, so it has to start with `./`.
    emit toToken prefix (table, files) global builder =
      let
        bytes = substitute table (render builder)
        hash = take 16 (SHA.showDigest (SHA.sha1 (LBS.fromStrict bytes)))
        name = base ++ "." ++ hash ++ ".mjs"
      in
      ( (toToken global, BS_UTF8.fromString (prefix ++ name)) : table
      , (name, bytes) : files
      )

    afterWorkers =
      List.foldl'
        (\acc (WorkerBundle global builder) -> emit Workers.token "" acc global builder)
        ([], []) workers

    (finalTable, revFiles) =
      List.foldl'
        (\acc (ChunkBundle home builder) -> emit Chunks.token "./" acc home builder)
        afterWorkers chunks
  in
  ( reverse revFiles
  , substitute finalTable (render js)
  , fmap render css
  )


-- Like finalize, but the caller names the worker and chunk files. The
-- reactor serves each at its own module's URL rather than as a hashed
-- sibling, so only the main bundle is rendered here; the file's own
-- request renders it. Left is something the caller could not name.
finalizeWith
  :: (Opt.Global -> Maybe String)
  -> (ModuleName.Canonical -> Maybe String)
  -> Bundles
  -> Either (Either Opt.Global ModuleName.Canonical) (BS.ByteString, Maybe BS.ByteString, [(ModuleName.Canonical, BS.ByteString)])
finalizeWith nameOfWorker nameOfChunk (Bundles js css workers _ chunks _) =
  do  workerTable <- traverse workerEntry workers
      chunkTable <- traverse chunkEntry chunks
      let table = workerTable ++ chunkTable
      return
        ( substitute table (render js)
        , fmap render css
        , map (\(ChunkBundle home builder) -> (home, substitute table (render builder))) chunks
        )
  where
    workerEntry (WorkerBundle global _) =
      case nameOfWorker global of
        Just name -> Right (Workers.token global, BS_UTF8.fromString name)
        Nothing   -> Left (Left global)

    chunkEntry (ChunkBundle home _) =
      case nameOfChunk home of
        Just name -> Right (Chunks.token home, BS_UTF8.fromString name)
        Nothing   -> Left (Right home)


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


repl :: R.Stuff -> Details.Details -> Bool -> Build.ReplArtifacts -> N.Name -> Task B.Builder
repl stuff details ansi (Build.ReplArtifacts home modules localizer annotations) name =
  do  objects <- finalizeObjects =<< loadObjects stuff details modules
      let graph = objectsToGlobalGraph objects
      return $ JS.generateForRepl ansi localizer graph home name $
        $(Map.require 'repl) name annotations N.toChars



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


lookupMain :: Pkg.Name -> Map.Map Module.Name Opt.LocalGraph -> Build.Root -> Maybe (ModuleName.Canonical, Opt.Main)
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
    { _foreign_mvar :: Fork.SafeMVar (Maybe Opt.GlobalGraph)
    , _local_mvars :: Map.Map Module.Name (Fork.SafeMVar (Maybe Opt.LocalGraph))
    }


loadObjects :: R.Stuff -> Details.Details -> [Build.Module] -> Task LoadingObjects
loadObjects stuff details modules =
  Task.io $
  do  mvar <- Details.loadObjects stuff details
      mvars <- traverse (loadObject stuff) modules
      return $ LoadingObjects mvar (Map.fromList mvars)


loadObject :: R.Stuff -> Build.Module -> IO (Module.Name, Fork.SafeMVar (Maybe Opt.LocalGraph))
loadObject stuff modul =
  case modul of
    Build.Fresh  name _ graph -> (,) name <$> Fork.cached (Just graph)
    Build.Cached name _ _     -> (,) name <$> Fork.fork name (File.readBytes Opt.dLocalGraph (R.elmo stuff name))



-- FINALIZE OBJECTS


data Objects =
  Objects
    { _foreign :: Opt.GlobalGraph
    , _locals :: Map.Map Module.Name Opt.LocalGraph
    }


finalizeObjects :: LoadingObjects -> Task Objects
finalizeObjects (LoadingObjects mvar mvars) =
  Task.eio id $
  do  result  <- Fork.await mvar
      results <- traverse Fork.await mvars
      case liftM2 Objects result (sequence results) of
        Just loaded -> return (Right loaded)
        Nothing     -> return (Left Exit.GenerateCannotLoadArtifacts)


objectsToGlobalGraph :: Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
  foldr Opt.addLocalGraph globals locals



-- LOAD TYPES


loadTypes :: R.Stuff -> Map.Map ModuleName.Canonical I.DependencyInterface -> [Build.Module] -> Task Extract.Types
loadTypes stuff ifaces modules =
  Task.eio id $
  do  mvars <- traverse (loadTypesHelp stuff) modules
      let !foreigns = Extract.mergeMany (Map.elems (Map.mapWithKey Extract.fromDependencyInterface ifaces))
      results <- traverse Fork.await mvars
      case sequence results of
        Just ts -> return (Right (Extract.merge foreigns (Extract.mergeMany ts)))
        Nothing -> return (Left Exit.GenerateCannotLoadArtifacts)


loadTypesHelp :: R.Stuff -> Build.Module -> IO (Fork.SafeMVar (Maybe Extract.Types))
loadTypesHelp stuff modul =
  case modul of
    Build.Fresh name iface _ ->
      Fork.cached $ Just $ Extract.fromInterface name iface

    Build.Cached name _ ciMVar ->
      do  cachedInterface <- readMVar ciMVar
          case cachedInterface of
            Build.Unneeded ->
              Fork.fork name $
                do  maybeIface <- File.readBytes I.dInterface (R.elmi stuff name)
                    return $ Extract.fromInterface name <$> maybeIface

            Build.Loaded iface ->
              Fork.cached $ Just (Extract.fromInterface name iface)

            Build.Corrupted ->
              Fork.cached Nothing
