{-# LANGUAGE OverloadedStrings #-}
module Develop
  ( Flags(..)
  , run
  )
  where


import Control.Applicative ((<|>))
import Control.Monad (filterM, guard)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir
import System.FilePath as FP
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe

import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Build
import qualified Elm.Details as Details
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Json.Encode as Json
import qualified Develop.Generate.Help as Help
import qualified Develop.Generate.Index as Index
import qualified Develop.StaticFiles as StaticFiles
import qualified Generate.Html as Html
import qualified Generate
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as ExitHelp
import qualified Reporting.Task as Task
import qualified Stuff



-- RUN THE DEV SERVER


data Flags =
  Flags
    { _port :: Maybe Int
    }


run :: () -> Flags -> IO ()
run () (Flags maybePort) =
  do  let port = maybe 8000 id maybePort
      putStrLn $ "Go to http://localhost:" ++ show port ++ " to see your project dashboard."
      httpServe (config port) $
        serveCompiled
        <|> serveFiles
        <|> serveDirectoryWith directoryConfig "."
        <|> serveAssets
        <|> error404


config :: Int -> Config Snap a
config port =
  setVerbose False $ setPort port $
    setAccessLog ConfigNoLog $ setErrorLog ConfigNoLog $ defaultConfig



-- INDEX


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
  fancyDirectoryConfig
    { indexFiles = []
    , indexGenerator = \pwd ->
        do  modifyResponse $ setContentType "text/html;charset=utf-8"
            writeBuilder =<< liftIO (Index.generate pwd)
    }



-- NOT FOUND


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html;charset=utf-8"
      writeBuilder $ Help.makePageHtml "NotFound" Nothing



-- SERVE FILES


serveFiles :: Snap ()
serveFiles =
  do  path <- getSafePath
      guard =<< liftIO (Dir.doesFileExist path)
      serveElm path <|> serveFilePretty path



-- SERVE FILES + CODE HIGHLIGHTING


serveFilePretty :: FilePath -> Snap ()
serveFilePretty path =
  let
    possibleExtensions =
      getSubExts (takeExtensions path)
  in
    case mconcat (map lookupMimeType possibleExtensions) of
      Nothing ->
        serveCode path

      Just mimeType ->
        serveFileAs mimeType path


getSubExts :: String -> [String]
getSubExts fullExtension =
  if null fullExtension then
    []
  else
    fullExtension : getSubExts (takeExtensions (drop 1 fullExtension))


serveCode :: String -> Snap ()
serveCode path =
  do  code <- liftIO (BS.readFile path)
      modifyResponse (setContentType "text/html")
      writeBuilder $
        Help.makeCodeHtml ('~' : '/' : path) (B.byteString code)



-- SERVE ELM


serveElm :: FilePath -> Snap ()
serveElm path =
  do  guard (takeExtension path == ".elm")
      modifyResponse (setContentType "text/html")
      result <- liftIO $ compile path
      case result of
        Right builder ->
          writeBuilder builder

        Left exit ->
          writeBuilder $ Help.makePageHtml "Errors" $ Just $
            Exit.toJson $ Exit.reactorToReport exit


compile :: FilePath -> IO (Either Exit.Reactor B.Builder)
compile path =
  build path $ \root details artifacts ->
    do  bundles <- generate Generate.Iife root details artifacts
        case bundles of
          Generate.Bundles _ _ _ True ->
            Task.throw (Exit.ReactorBadGenerate Exit.GenerateScriptBadOutput)

          -- spawns workers: only a module knows its own URL, so the page
          -- loads the program from the .mjs endpoint instead of inlining it
          Generate.Bundles _ css (_:_) _ ->
            do  let (NE.List name _) = Build.getRootNames artifacts
                return $ Html.sandwichModule name css (B.stringUtf8 ('/' : path ++ ".mjs"))

          Generate.Bundles javascript css [] _ ->
            do  let (NE.List name _) = Build.getRootNames artifacts
                return $ Html.sandwich name css javascript


-- Load the project and build one file, then hand the result to whatever
-- wants to generate from it.
build :: FilePath -> (FilePath -> Details.Details -> Build.Artifacts -> Task.Task Exit.Reactor a) -> IO (Either Exit.Reactor a)
build path continue =
  do  maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing ->
          return $ Left $ Exit.ReactorNoOutline

        Just root ->
          BW.withScope $ \scope -> Stuff.withRootLock root $ Task.run $
            do  details <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
                artifacts <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details (NE.List path [])
                continue root details artifacts


generate :: Generate.Format -> FilePath -> Details.Details -> Build.Artifacts -> Task.Task Exit.Reactor Generate.Bundles
generate format root details artifacts =
  Task.mapError Exit.ReactorBadGenerate $ Generate.dev format root details artifacts



-- SERVE COMPILED PIECES
--
-- `Main.elm.js`, `Main.elm.css` and `Main.elm.mjs` compile `Main.elm` on
-- request and serve one piece of the result, so a hand-written HTML page can
-- pull in exactly what `elm make` would have written -- its own <meta
-- viewport>, its own ports, a module bundle that spawns workers. Each worker
-- is served at its own module's URL, so `Counter.elm.mjs` compiles the worker
-- program in `Counter.elm`; no hashed sibling files are involved.


data Piece
  = Js
  | Css
  | Mjs


serveCompiled :: Snap ()
serveCompiled =
  do  path <- getSafePath
      case compiledRequest path of
        Nothing ->
          pass

        Just (elmPath, piece) ->
          do  guard =<< liftIO (Dir.doesFileExist elmPath)
              result <- liftIO (compilePiece piece elmPath)
              -- a worker's URL is stable, which is exactly what a browser would
              -- otherwise cache across edits
              modifyResponse (setHeader "Cache-Control" "no-store")
              case result of
                Right bytes ->
                  do  modifyResponse (setContentType (pieceMime piece))
                      writeBS bytes

                Left exit ->
                  do  modifyResponse (setResponseStatus 500 "Internal Server Error")
                      modifyResponse (setContentType (pieceMime piece))
                      case piece of
                        Css -> return ()          -- nowhere to put words in a stylesheet
                        _   -> writeBuilder (errorScript exit)


compiledRequest :: FilePath -> Maybe (FilePath, Piece)
compiledRequest path =
  stripSuffix ".elm.mjs" Mjs <|> stripSuffix ".elm.css" Css <|> stripSuffix ".elm.js" Js
  where
    stripSuffix suffix piece =
      if suffix `List.isSuffixOf` path
        then Just (take (length path - length suffix) path ++ ".elm", piece)
        else Nothing


pieceMime :: Piece -> BS.ByteString
pieceMime piece =
  case piece of
    Js  -> "text/javascript;charset=utf-8"
    Mjs -> "text/javascript;charset=utf-8"
    Css -> "text/css;charset=utf-8"


-- A failed build served as a script logs the compiler's report where the
-- developer is already looking, instead of arriving as a silent HTML page.
errorScript :: Exit.Reactor -> B.Builder
errorScript exit =
  "console.error("
    <> Json.encodeUgly (Json.chars (ExitHelp.toString (ExitHelp.reportToDoc (Exit.reactorToReport exit))))
    <> ");\n"


compilePiece :: Piece -> FilePath -> IO (Either Exit.Reactor BS.ByteString)
compilePiece piece path =
  do  served <- Dir.canonicalizePath =<< Dir.getCurrentDirectory
      build path $ \root details artifacts -> compilePieceIn served root details artifacts piece


compilePieceIn :: FilePath -> FilePath -> Details.Details -> Build.Artifacts -> Piece -> Task.Task Exit.Reactor BS.ByteString
compilePieceIn served root details artifacts piece =
  case piece of
    Js ->
      do  bundles <- generate Generate.Iife root details artifacts
          case bundles of
            Generate.Bundles _ _ _ True ->
              Task.throw (Exit.ReactorBadGenerate Exit.GenerateScriptBadOutput)

            Generate.Bundles _ _ (_:_) _ ->
              Task.throw (Exit.ReactorBadGenerate Exit.GenerateWorkersRequireEsm)

            Generate.Bundles javascript _ [] _ ->
              return (toBytes javascript)

    Css ->
      do  Generate.Bundles _ css _ _ <- generate Generate.Iife root details artifacts
          return (maybe BS.empty toBytes css)

    Mjs ->
      do  bundles <- generate Generate.Esm root details artifacts
          if Generate._isScript bundles
            then Task.throw (Exit.ReactorBadGenerate Exit.GenerateScriptBadOutput)
            else
              do  let workers = [ global | Generate.WorkerBundle global _ <- Generate._workerBundles bundles ]
                  urls <- Task.io (workerUrls served root details workers)
                  case Generate.finalizeWith (\global -> Map.findWithDefault Nothing global urls) bundles of
                    Left (Opt.Global home _) ->
                      Task.throw (Exit.ReactorWorkerUnservable (ModuleName._module home))

                    Right (javascript, _) ->
                      return javascript


-- Each worker is served at its own module's URL, as an absolute path so it
-- resolves against the origin whatever directory the spawner sits in. The
-- module is found the way the builder finds it, by looking for its file under
-- each source directory; the cached module table cannot be used for this,
-- since it is read before the build and so knows nothing of a fresh elm-stuff.
-- The reactor serves the directory it was started in, so the URL is the path
-- relative to that; a worker outside it, in a package or under a source
-- directory elsewhere, cannot be served, and makeRelative leaves it absolute.
workerUrls :: FilePath -> FilePath -> Details.Details -> [Opt.Global] -> IO (Map.Map Opt.Global (Maybe String))
workerUrls served root details globals =
  do  dirs <- traverse (Dir.canonicalizePath . toAbsolute root) (sourceDirs details)
      Map.fromList <$> traverse (\global -> (,) global <$> workerUrl served dirs global) globals


workerUrl :: FilePath -> [FilePath] -> Opt.Global -> IO (Maybe String)
workerUrl served dirs (Opt.Global home _) =
  do  let file = ModuleName.toFilePath (ModuleName._module home) <.> "elm"
      hits <- filterM (\folder -> Dir.doesFileExist (folder </> file)) dirs
      return $
        case hits of
          folder : _ ->
            let relative = FP.makeRelative served (folder </> file) in
            if FP.isAbsolute relative then Nothing else Just ('/' : relative ++ ".mjs")

          [] ->
            Nothing


sourceDirs :: Details.Details -> [Outline.SrcDir]
sourceDirs details =
  case Details._outline details of
    Details.ValidApp dirs -> NE.toList dirs
    Details.ValidPkg _ _ _ -> [Outline.RelativeSrcDir "src"]


toAbsolute :: FilePath -> Outline.SrcDir -> FilePath
toAbsolute root srcDir =
  case srcDir of
    Outline.AbsoluteSrcDir folder -> folder
    Outline.RelativeSrcDir folder -> root </> folder


toBytes :: B.Builder -> BS.ByteString
toBytes =
  LBS.toStrict . B.toLazyByteString



-- SERVE STATIC ASSETS


serveAssets :: Snap ()
serveAssets =
  do  path <- getSafePath
      case StaticFiles.lookup path of
        Nothing ->
          pass

        Just (content, mimeType) ->
          do  modifyResponse (setContentType (mimeType <> ";charset=utf-8"))
              writeBS content



-- MIME TYPES


lookupMimeType :: FilePath -> Maybe BS.ByteString
lookupMimeType ext =
  HashMap.lookup ext mimeTypeDict


(==>) :: a -> b -> (a,b)
(==>) a b =
  (a, b)


mimeTypeDict :: HashMap.HashMap FilePath BS.ByteString
mimeTypeDict =
  HashMap.fromList
    [ ".asc"     ==> "text/plain"
    , ".asf"     ==> "video/x-ms-asf"
    , ".asx"     ==> "video/x-ms-asf"
    , ".avi"     ==> "video/x-msvideo"
    , ".bz2"     ==> "application/x-bzip"
    , ".css"     ==> "text/css"
    , ".dtd"     ==> "text/xml"
    , ".dvi"     ==> "application/x-dvi"
    , ".gif"     ==> "image/gif"
    , ".gz"      ==> "application/x-gzip"
    , ".htm"     ==> "text/html"
    , ".html"    ==> "text/html"
    , ".ico"     ==> "image/x-icon"
    , ".jpeg"    ==> "image/jpeg"
    , ".jpg"     ==> "image/jpeg"
    , ".js"      ==> "text/javascript"
    , ".mjs"     ==> "text/javascript"
    , ".json"    ==> "application/json"
    , ".m3u"     ==> "audio/x-mpegurl"
    , ".mov"     ==> "video/quicktime"
    , ".mp3"     ==> "audio/mpeg"
    , ".mp4"     ==> "video/mp4"
    , ".mpeg"    ==> "video/mpeg"
    , ".mpg"     ==> "video/mpeg"
    , ".ogg"     ==> "application/ogg"
    , ".otf"     ==> "font/otf"
    , ".pac"     ==> "application/x-ns-proxy-autoconfig"
    , ".pdf"     ==> "application/pdf"
    , ".png"     ==> "image/png"
    , ".qt"      ==> "video/quicktime"
    , ".sfnt"    ==> "font/sfnt"
    , ".sig"     ==> "application/pgp-signature"
    , ".spl"     ==> "application/futuresplash"
    , ".svg"     ==> "image/svg+xml"
    , ".swf"     ==> "application/x-shockwave-flash"
    , ".tar"     ==> "application/x-tar"
    , ".tar.bz2" ==> "application/x-bzip-compressed-tar"
    , ".tar.gz"  ==> "application/x-tgz"
    , ".tbz"     ==> "application/x-bzip-compressed-tar"
    , ".text"    ==> "text/plain"
    , ".tgz"     ==> "application/x-tgz"
    , ".ttf"     ==> "font/ttf"
    , ".txt"     ==> "text/plain"
    , ".wav"     ==> "audio/x-wav"
    , ".wax"     ==> "audio/x-ms-wax"
    , ".webm"    ==> "video/webm"
    , ".webp"    ==> "image/webp"
    , ".wma"     ==> "audio/x-ms-wma"
    , ".wmv"     ==> "video/x-ms-wmv"
    , ".woff"    ==> "font/woff"
    , ".woff2"   ==> "font/woff2"
    , ".xbm"     ==> "image/x-xbitmap"
    , ".xml"     ==> "text/xml"
    , ".xpm"     ==> "image/x-xpixmap"
    , ".xwd"     ==> "image/x-xwindowdump"
    , ".zip"     ==> "application/zip"
    ]
