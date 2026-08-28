{-# LANGUAGE OverloadedStrings #-}
module Make
  ( Flags(..)
  , Output(..)
  , ReportType(..)
  , run
  , reportType
  , output
  , docsFile
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir
import qualified System.FilePath as FP

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified Build
import qualified Elm.Details as Details
import qualified File
import qualified Generate
import qualified Generate.Html as Html
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Root as R
import Terminal (Parser(..))



-- FLAGS


data Flags =
  Flags
    { _debug :: Bool
    , _optimize :: Bool
    , _output :: Maybe Output
    , _report :: Maybe ReportType
    , _docs :: Maybe FilePath
    }


data Output
  = JS FilePath
  | Esm FilePath
  | Html FilePath
  | DevNull


data ReportType
  = Json



-- RUN


type Task a = Task.Task Exit.Make a


run :: [FilePath] -> Flags -> IO ()
run paths flags@(Flags _ _ _ report _) =
  do  style <- getStyle report
      maybeRoot <- R.findRoot
      Reporting.attemptWithStyle style Exit.makeToReport $
        case maybeRoot of
          Just root -> runHelp root paths style flags
          Nothing   -> return $ Left $ Exit.MakeNoOutline


runHelp :: R.Root -> [FilePath] -> Reporting.Style -> Flags -> IO (Either Exit.Make ())
runHelp root paths style (Flags debug optimize maybeOutput _ maybeDocs) =
  R.withRootLock root $ \writer stuff ->
  Task.run $
  do  desiredMode <- getMode debug optimize
      details <- Task.eio Exit.MakeBadDetails (Details.load writer style root stuff)
      case paths of
        [] ->
          do  exposed <- getExposed details
              buildExposed writer style root stuff details maybeDocs exposed

        p:ps ->
          do  artifacts <- buildPaths writer style root stuff details (NE.List p ps)
              case maybeOutput of
                Nothing ->
                  case getMains artifacts of
                    [] ->
                      return ()

                    [name] ->
                      do  bundles <- noWorkers =<< toBuilder Generate.Iife stuff details desiredMode artifacts
                          let (Generate.Bundles builder css _) = bundles
                          generate writer style "index.html" (Html.sandwich name css builder) (NE.List name [])

                    name:names ->
                      do  bundles <- noWorkers =<< toBuilder Generate.Iife stuff details desiredMode artifacts
                          let (Generate.Bundles builder css _) = bundles
                          writeCss writer "elm.js" css
                          generate writer style "elm.js" builder (NE.List name names)

                Just DevNull ->
                  return ()

                Just (JS target) ->
                  case getNoMains artifacts of
                    [] ->
                      do  bundles <- noWorkers =<< toBuilder Generate.Iife stuff details desiredMode artifacts
                          let (Generate.Bundles builder css _) = bundles
                          writeCss writer target css
                          generate writer style target builder (Build.getRootNames artifacts)

                    name:names ->
                      Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)

                Just (Esm target) ->
                  case getNoMains artifacts of
                    [] ->
                      do  bundles <- toBuilder Generate.Esm stuff details desiredMode artifacts
                          writeBundles writer style target bundles (Build.getRootNames artifacts)

                    name:names ->
                      Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)

                Just (Html target) ->
                  do  name <- hasOneMain artifacts
                      bundles <- noWorkers =<< toBuilder Generate.Iife stuff details desiredMode artifacts
                      let (Generate.Bundles builder css _) = bundles
                      generate writer style target (Html.sandwich name css builder) (NE.List name [])



-- GET INFORMATION


getStyle :: Maybe ReportType -> IO Reporting.Style
getStyle report =
  case report of
    Nothing -> Reporting.terminal
    Just Json -> return Reporting.json


getMode :: Bool -> Bool -> Task DesiredMode
getMode debug optimize =
  case (debug, optimize) of
    (True , True ) -> Task.throw Exit.MakeCannotOptimizeAndDebug
    (True , False) -> return Debug
    (False, False) -> return Dev
    (False, True ) -> return Prod


getExposed :: Details.Details -> Task (NE.List Module.Name)
getExposed (Details.Details _ validOutline _ _ _ _) =
  case validOutline of
    Details.ValidApp _ ->
      Task.throw Exit.MakeAppNeedsFileNames

    Details.ValidPkg _ exposed _ ->
      case exposed of
        [] -> Task.throw Exit.MakePkgNeedsExposing
        m:ms -> return (NE.List m ms)



-- BUILD PROJECTS


buildExposed :: File.Writer R.PROJECT -> Reporting.Style -> R.Root -> R.Stuff -> Details.Details -> Maybe FilePath -> NE.List Module.Name -> Task ()
buildExposed writer style root stuff details maybeDocs exposed =
  let
    docsGoal = maybe Build.IgnoreDocs Build.WriteDocs maybeDocs
  in
  Task.eio Exit.MakeCannotBuild $
    Build.fromExposed writer style root stuff details docsGoal exposed


buildPaths :: File.Writer R.PROJECT -> Reporting.Style -> R.Root -> R.Stuff -> Details.Details -> NE.List FilePath -> Task Build.Artifacts
buildPaths writer style root stuff details paths =
  Task.eio Exit.MakeCannotBuild $
    Build.fromPaths writer style root stuff details paths



-- GET MAINS


getMains :: Build.Artifacts -> [Module.Name]
getMains (Build.Artifacts _ _ roots modules) =
  Maybe.mapMaybe (getMain modules) (NE.toList roots)


getMain :: [Build.Module] -> Build.Root -> Maybe Module.Name
getMain modules root =
  case root of
    Build.Inside name ->
      if any (isMain name) modules
      then Just name
      else Nothing

    Build.Outside name _ (Opt.LocalGraph maybeMain _ _) ->
      case maybeMain of
        Just _  -> Just name
        Nothing -> Nothing


isMain :: Module.Name -> Build.Module -> Bool
isMain targetName modul =
  case modul of
    Build.Fresh name _ (Opt.LocalGraph maybeMain _ _) ->
      Maybe.isJust maybeMain && name == targetName

    Build.Cached name mainIsDefined _ ->
      mainIsDefined && name == targetName



-- HAS ONE MAIN


hasOneMain :: Build.Artifacts -> Task Module.Name
hasOneMain (Build.Artifacts _ _ roots modules) =
  case roots of
    NE.List root [] -> Task.mio Exit.MakeNoMain (return $ getMain modules root)
    NE.List _ (_:_) -> Task.throw Exit.MakeMultipleFilesIntoHtml



-- GET MAINLESS


getNoMains :: Build.Artifacts -> [Module.Name]
getNoMains (Build.Artifacts _ _ roots modules) =
  Maybe.mapMaybe (getNoMain modules) (NE.toList roots)


getNoMain :: [Build.Module] -> Build.Root -> Maybe Module.Name
getNoMain modules root =
  case root of
    Build.Inside name ->
      if any (isMain name) modules
      then Nothing
      else Just name

    Build.Outside name _ (Opt.LocalGraph maybeMain _ _) ->
      case maybeMain of
        Just _  -> Nothing
        Nothing -> Just name



-- GENERATE


generate :: File.Writer R.PROJECT -> Reporting.Style -> FilePath -> B.Builder -> NE.List Module.Name -> Task ()
generate writer style target builder names =
  Task.io $
    do  Dir.createDirectoryIfMissing True (FP.takeDirectory target)
        File.writeBuilder writer target builder
        Reporting.reportGenerate style names target


-- Non-ESM outputs cannot host web workers (no import.meta to resolve the
-- worker files relative to the bundle).
noWorkers :: Generate.Bundles -> Task Generate.Bundles
noWorkers bundles@(Generate.Bundles _ _ workers) =
  case workers of
    [] -> return bundles
    _ -> Task.throw (Exit.MakeBadGenerate Exit.GenerateWorkersRequireEsm)


-- ESM output: the main bundle, its .css sidecar, and one .mjs file per
-- spawned worker program, named by content hash.
writeBundles :: File.Writer R.PROJECT -> Reporting.Style -> FilePath -> Generate.Bundles -> NE.List Module.Name -> Task ()
writeBundles writer style target bundles names =
  Task.io $
    do  let dir = FP.takeDirectory target
        Dir.createDirectoryIfMissing True dir
        let (workerFiles, mainBytes, cssBytes) = Generate.finalize (FP.takeBaseName target) bundles
        mapM_ (\(name, bytes) -> File.writeBuilder writer (dir FP.</> name) (B.byteString bytes)) workerFiles
        maybe (return ()) (File.writeBuilder writer (target ++ ".css") . B.byteString) cssBytes
        File.writeBuilder writer target (B.byteString mainBytes)
        Reporting.reportGenerate style names target


-- Write the sidecar stylesheet next to the JS output, e.g. `elm.mjs.css`
-- for `--output=elm.mjs`. Only written when the program has CSS blocks.
writeCss :: File.Writer R.PROJECT -> FilePath -> Maybe B.Builder -> Task ()
writeCss writer target maybeCss =
  case maybeCss of
    Nothing ->
      return ()

    Just css ->
      Task.io $
        do  Dir.createDirectoryIfMissing True (FP.takeDirectory target)
            File.writeBuilder writer (target ++ ".css") css



-- TO BUILDER


data DesiredMode = Debug | Dev | Prod


toBuilder :: Generate.Format -> R.Stuff -> Details.Details -> DesiredMode -> Build.Artifacts -> Task Generate.Bundles
toBuilder format stuff details desiredMode artifacts =
  Task.mapError Exit.MakeBadGenerate $
    case desiredMode of
      Debug -> Generate.debug format stuff details artifacts
      Dev   -> Generate.dev   format stuff details artifacts
      Prod  -> Generate.prod  format stuff details artifacts



-- PARSERS


reportType :: Parser ReportType
reportType =
  Parser
    { _singular = "report type"
    , _plural = "report types"
    , _parser = \string -> if string == "json" then Just Json else Nothing
    , _suggest = \_ -> return ["json"]
    , _examples = \_ -> return ["json"]
    }


output :: Parser Output
output =
  Parser
    { _singular = "output file"
    , _plural = "output files"
    , _parser = parseOutput
    , _suggest = \_ -> return []
    , _examples = \_ -> return [ "elm.js", "elm.mjs", "index.html", "/dev/null" ]
    }


parseOutput :: String -> Maybe Output
parseOutput name
  | isDevNull name      = Just DevNull
  | hasExt ".html" name = Just (Html name)
  | hasExt ".js"   name = Just (JS name)
  | hasExt ".mjs"  name = Just (Esm name)
  | otherwise           = Nothing


docsFile :: Parser FilePath
docsFile =
  Parser
    { _singular = "json file"
    , _plural = "json files"
    , _parser = \name -> if hasExt ".json" name then Just name else Nothing
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["docs.json","documentation.json"]
    }


hasExt :: String -> String -> Bool
hasExt ext path =
  FP.takeExtension path == ext && length path > length ext


isDevNull :: String -> Bool
isDevNull name =
  name == "/dev/null" || name == "NUL" || name == "$null"
