{-# LANGUAGE BangPatterns, TemplateHaskell #-}
module Generate
  ( Format(..)
  , debug
  , dev
  , prod
  , repl
  )
  where


import Prelude hiding (cycle, print)
import Control.Concurrent (readMVar)
import Control.Monad (liftM2)
import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE

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


generateWith :: Format -> Mode.Mode -> Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> (B.Builder, Maybe B.Builder)
generateWith format =
  case format of
    Iife -> JS.generate
    Esm  -> JS.generateEsm


debug :: Format -> R.Stuff -> Details.Details -> Build.Artifacts -> Task (B.Builder, Maybe B.Builder)
debug format stuff details (Build.Artifacts pkg ifaces roots modules) =
  do  loading <- loadObjects stuff details modules
      types   <- loadTypes stuff ifaces modules
      objects <- finalizeObjects loading
      let mode = Mode.Dev (Just types)
      let graph = objectsToGlobalGraph objects
      let mains = gatherMains pkg objects roots
      return $ generateWith format mode graph mains


dev :: Format -> R.Stuff -> Details.Details -> Build.Artifacts -> Task (B.Builder, Maybe B.Builder)
dev format stuff details (Build.Artifacts pkg _ roots modules) =
  do  objects <- finalizeObjects =<< loadObjects stuff details modules
      let mode = Mode.Dev Nothing
      let graph = objectsToGlobalGraph objects
      let mains = gatherMains pkg objects roots
      return $ generateWith format mode graph mains


prod :: Format -> R.Stuff -> Details.Details -> Build.Artifacts -> Task (B.Builder, Maybe B.Builder)
prod format stuff details (Build.Artifacts pkg _ roots modules) =
  do  objects <- finalizeObjects =<< loadObjects stuff details modules
      checkForDebugUses objects
      let graph = objectsToGlobalGraph objects
      let mains = gatherMains pkg objects roots
      let mode = Mode.Prod (Mode.ShortNames (Mode.shortenFieldNames graph) (GenCss.shortenNames graph mains))
      return $ generateWith format mode graph mains


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
