{-# LANGUAGE OverloadedStrings #-}
module Deps.Git
  ( Problem(..)
  , getVersions
  , ensurePackage
  )
  where


import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as SysExit
import System.FilePath ((</>), takeDirectory)
import qualified System.Process as Process

import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A



-- PROBLEM


data Problem
  = MissingGit
  | CommandFailed String String
  | NoVersions String
  | MissingOutline Pkg.Name V.Version String
  | LocalCacheConflict Pkg.Name V.Version String (Maybe String)



-- RUN GIT


run :: [String] -> IO (Either Problem String)
run args =
  do  maybeGit <- Dir.findExecutable "git"
      case maybeGit of
        Nothing ->
          return (Left MissingGit)

        Just git ->
          do  environment <- Env.getEnvironment
              let process =
                    (Process.proc git args)
                      { Process.env =
                          Just $ ("GIT_TERMINAL_PROMPT","0")
                            : filter ((/=) "GIT_TERMINAL_PROMPT" . fst) environment
                      }
              (exit, out, errOut) <- Process.readCreateProcessWithExitCode process ""
              case exit of
                SysExit.ExitSuccess ->
                  return (Right out)

                SysExit.ExitFailure _ ->
                  return (Left (CommandFailed (unwords ("git" : args)) errOut))



-- GET VERSIONS
--
-- List the version tags available in the remote repository. Only tags
-- that look exactly like Elm versions (e.g. "2.0.1") are considered.
--


getVersions :: String -> IO (Either Problem [V.Version])
getVersions url =
  do  result <- run ["ls-remote", "--tags", "--refs", url]
      case result of
        Left problem ->
          return (Left problem)

        Right output ->
          do  versions <- traverse parseTagLine (lines output)
              case Maybe.catMaybes versions of
                [] -> return (Left (NoVersions url))
                vs -> return (Right (List.sortBy (flip compare) vs))


parseTagLine :: String -> IO (Maybe V.Version)
parseTagLine line =
  case break (== '\t') line of
    (_, '\t' : ref) ->
      case List.stripPrefix "refs/tags/" ref of
        Just tag ->
          do  result <- P.fromByteString V.parser A.Position (BS.pack tag)
              case result of
                Right vsn | V.toChars vsn == tag -> return (Just vsn)
                _                                -> return Nothing

        Nothing ->
          return Nothing

    _ ->
      return Nothing



-- ENSURE PACKAGE
--
-- Make sure ELM_HOME/.../packages/author/project/version is populated
-- from the given git URL. The version must exist as a tag in the remote
-- repository. A "git-url" file records where the sources came from, so
-- that two projects mapping the same package name to different URLs do
-- not silently share one cache entry.
--


ensurePackage :: FilePath -> Pkg.Name -> String -> V.Version -> IO (Either Problem ())
ensurePackage home pkg url vsn =
  do  outlineExists <- Dir.doesFileExist (home </> "elm.json")
      if outlineExists
        then checkOrigin home pkg url vsn
        else populate home pkg url vsn


checkOrigin :: FilePath -> Pkg.Name -> String -> V.Version -> IO (Either Problem ())
checkOrigin home pkg url vsn =
  do  let originPath = home </> "git-url"
      originExists <- Dir.doesFileExist originPath
      if not originExists
        then return (Left (LocalCacheConflict pkg vsn url Nothing))
        else
          do  origin <- readFile originPath
              if origin == url
                then return (Right ())
                else return (Left (LocalCacheConflict pkg vsn url (Just origin)))


populate :: FilePath -> Pkg.Name -> String -> V.Version -> IO (Either Problem ())
populate home pkg url vsn =
  do  let cloneDir = home ++ ".git-clone"
      let stageDir = home ++ ".git-stage"
      removeIfExists cloneDir
      removeIfExists stageDir
      Dir.createDirectoryIfMissing True (takeDirectory home)
      result <-
        run
          [ "clone", "--depth", "1", "--branch", V.toChars vsn
          , "--config", "advice.detachedHead=false"
          , "--", url, cloneDir
          ]
      case result of
        Left problem ->
          return (Left problem)

        Right _ ->
          do  hasOutline <- Dir.doesFileExist (cloneDir </> "elm.json")
              hasSrc <- Dir.doesDirectoryExist (cloneDir </> "src")
              if not (hasOutline && hasSrc)
                then
                  do  removeIfExists cloneDir
                      return (Left (MissingOutline pkg vsn url))
                else
                  do  Dir.createDirectoryIfMissing True stageDir
                      copyDir (cloneDir </> "src") (stageDir </> "src")
                      copyFileIfExists cloneDir stageDir "elm.json"
                      copyFileIfExists cloneDir stageDir "LICENSE"
                      copyFileIfExists cloneDir stageDir "README.md"
                      writeFile (stageDir </> "git-url") url
                      Dir.renameDirectory stageDir home
                      removeIfExists cloneDir
                      return (Right ())



-- FILE HELPERS


removeIfExists :: FilePath -> IO ()
removeIfExists dir =
  do  exists <- Dir.doesDirectoryExist dir
      if exists
        then Dir.removePathForcibly dir
        else return ()


copyFileIfExists :: FilePath -> FilePath -> FilePath -> IO ()
copyFileIfExists from to name =
  do  exists <- Dir.doesFileExist (from </> name)
      if exists
        then Dir.copyFile (from </> name) (to </> name)
        else return ()


copyDir :: FilePath -> FilePath -> IO ()
copyDir from to =
  do  Dir.createDirectoryIfMissing True to
      names <- Dir.listDirectory from
      mapM_ (copyDirHelp from to) names


copyDirHelp :: FilePath -> FilePath -> FilePath -> IO ()
copyDirHelp from to name =
  do  let fromPath = from </> name
      let toPath = to </> name
      isDir <- Dir.doesDirectoryExist fromPath
      if isDir
        then copyDir fromPath toPath
        else Dir.copyFile fromPath toPath
