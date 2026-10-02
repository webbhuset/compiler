{-# LANGUAGE MagicHash #-}
module SourceMaps
  ( resolver
  )
  where


import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar)
import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))
import qualified System.FilePath as FP

import qualified AST.Source as Src
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Elm.Details as Details
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Generate.SourceMap as SourceMap
import qualified Parse.Module as Parse
import qualified Reporting.Annotation as A
import qualified Root as R



-- RESOLVER
--
-- Where each definition the generated files contain was written. Only the
-- modules the files actually contain are read and parsed; the source map
-- has no other use for them. Labels are paths relative to the directory
-- the files are written to for project modules, and `elm-packages/...`
-- for dependencies, whose sources are embedded in the map anyway.


resolver
  :: R.Root -> Details.Details -> FilePath
  -> [SourceMap.Origin]
  -> IO ([SourceMap.Source], Map.Map SourceMap.Origin SourceMap.Target)
resolver root details outputDir origins =
  do  versions <- packageVersions root details
      cache <- R.getPackageCache
      outDir <- Dir.makeAbsolute outputDir
      let modules = Set.toList $ Set.fromList [ (SourceMap._package o, SourceMap._module o) | o <- origins ]
      found <- concurrently (locate root details versions cache outDir) modules
      let located = [ (key, file) | (key, Just file) <- zip modules found ]
          sources = [ SourceMap.Source label content | (_, (label, content, _)) <- located ]
          index = Map.fromList (zip (map fst located) [0..])
          lines' = Map.fromList [ (key, starts) | (key, (_, _, starts)) <- located ]
          target o =
            do  let key = (SourceMap._package o, SourceMap._module o)
                i <- Map.lookup key index
                starts <- Map.lookup key lines'
                case starts of
                  Kernel line -> Just (SourceMap.Target i line True Nothing)
                  Elm table   -> (\l -> SourceMap.Target i l False (Just (SourceMap._module o))) <$> Map.lookup (unprefixed (SourceMap._name o)) table
      return (sources, Map.fromList [ (o, t) | o <- origins, Just t <- [target o] ])


-- A group of mutually recursive definitions is one global named after its
-- first member, with a prefix no Elm name can have; see
-- Canonicalize.Expression.fromManyNames.
unprefixed :: String -> String
unprefixed name =
  case name of
    '_' : 'M' : '$' : rest | not (null rest) -> rest
    _                                        -> name


data Starts
  = Elm (Map.Map String Int)
  | Kernel Int



-- LOCATE


locate
  :: R.Root -> Details.Details -> Map.Map Pkg.Name V.Version -> R.PackageCache -> FilePath
  -> (String, String)
  -> IO (Maybe (String, BS.ByteString, Starts))
locate root details@(Details.Details _ outline _ _ _ _) versions cache outDir (pkgName, home) =
  let
    modulePath = map (\c -> if c == '.' then FP.pathSeparator else c) home

    packageSrc pkg =
      (\vsn -> (pkg, vsn, R.package cache pkg vsn </> "src")) <$> Map.lookup pkg versions

    byChars = Map.fromList [ (Pkg.toChars p, p) | p <- Map.keys versions ]

    projectDirs = sourceDirs root details

    isKernel = pkgName == Pkg.toChars Pkg.kernel

    projectPackage =
      case outline of
        Details.ValidPkg pkg _ _ -> Pkg.toChars pkg
        Details.ValidApp _       -> Pkg.toChars Pkg.dummyName

    candidates
      | isKernel =
          [ (packageLabel pkg vsn kernelFile, dir </> kernelFile)
          | Just (pkg, vsn, dir) <- map packageSrc (Map.keys versions)
          ]
          ++ [ (relativeLabel (dir </> kernelFile), dir </> kernelFile) | dir <- projectDirs ]
      | pkgName == projectPackage =
          [ (relativeLabel (dir </> modulePath <.> "elm"), dir </> modulePath <.> "elm") | dir <- projectDirs ]
      | otherwise =
          [ (packageLabel pkg vsn (modulePath <.> "elm"), dir </> modulePath <.> "elm")
          | Just (pkg, vsn, dir) <- [packageSrc =<< Map.lookup pkgName byChars]
          ]

    kernelFile = "Elm" </> "Kernel" </> modulePath <.> "js"

    relativeLabel path = FP.makeRelative outDir path `orRelative` path
    orRelative rel path = if FP.isAbsolute rel then relativeFrom outDir path else rel

    packageLabel pkg vsn file =
      "elm-packages" </> Pkg.toChars pkg </> V.toChars vsn </> file

    projectType =
      case (outline, Map.lookup pkgName byChars) of
        (Details.ValidPkg pkg _ _, _) | pkgName == projectPackage -> Parse.Package pkg
        (_, Just pkg) | pkgName /= projectPackage -> Parse.Package pkg
        _ -> Parse.Application
  in
  do  existing <- filterM' (Dir.doesFileExist . snd) candidates
      case existing of
        [] ->
          return Nothing

        (label, path) : _ ->
          do  content <- File.readUtf8 path
              if isKernel
                then return (Just (label, content, Kernel (headerEnd content)))
                else
                  do  parsed <- Parse.fromByteString projectType content
                      -- forced here, so that it is done on this module's thread
                      starts <- Exception.evaluate (forceLines (either (const Map.empty) definitionLines parsed))
                      return $ Just (label, content, Elm starts)


-- `../src/Main.elm` from the output directory.
relativeFrom :: FilePath -> FilePath -> FilePath
relativeFrom from to =
  let
    a = FP.splitDirectories from
    b = FP.splitDirectories to
    common = length (takeWhile id (zipWith (==) a b))
  in
  FP.joinPath (replicate (length a - common) ".." ++ drop common b)


-- The zero-based line a kernel file's code starts on: the one its header
-- comment closes on.
headerEnd :: BS.ByteString -> Int
headerEnd content =
  case BS.breakSubstring (BSC.pack "*/") content of
    (before, _) -> BSC.count '\n' before


-- The zero-based line each top level name is defined on. Constructors and
-- record constructors are named like the values they compile to.
definitionLines :: Src.Module -> Map.Map String Int
definitionLines (Src.Module _ _ _ _ values unions aliases tags _ _ effects) =
  let
    line (A.Region start _) = fst (A.toEditorRowCol start) - 1
  in
  Map.fromList $
    [ (N.toChars n, line r) | A.At _ (Src.Value (A.At r n) _ _ _) <- values ]
    ++ [ (N.toChars n, line r) | A.At _ (Src.Union _ _ ctors) <- unions, (A.At r n, _) <- ctors ]
    ++ [ (T.nameToChars n, line r) | A.At _ (Src.Alias (A.At r n) _ _) <- aliases ]
    ++ [ (N.toChars n, line r) | A.At _ (Src.TagDecl (A.At r n) _) <- tags ]
    ++ [ (N.toChars n, line r) | Src.Ports ports <- [effects], Src.Port (A.At r n) _ <- ports ]



forceLines :: Map.Map String Int -> Map.Map String Int
forceLines table =
  Map.foldl' (flip seq) () table `seq` table



-- PROJECT


sourceDirs :: R.Root -> Details.Details -> [FilePath]
sourceDirs root (Details.Details _ outline _ _ _ _) =
  case outline of
    Details.ValidApp dirs  -> map (R.toAbsolutePath root) (NE.toList dirs)
    Details.ValidPkg _ _ _ -> [R.toAbsolutePath root (R.Relative "src")]


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


-- Every module is read and parsed on a thread of its own.
concurrently :: (a -> IO b) -> [a] -> IO [b]
concurrently work xs =
  do  vars <- traverse fork xs
      traverse (\var -> either rethrow return =<< readMVar var) vars
  where
    fork x =
      do  var <- newEmptyMVar
          _ <- forkIO (putMVar var =<< Exception.try (work x))
          return var

    rethrow :: Exception.SomeException -> IO b
    rethrow = Exception.throwIO


filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' p xs =
  map fst . filter snd . zip xs <$> traverse p xs

