{-# LANGUAGE TemplateHaskell #-}
module BuildStamp
  ( commitCount
  )
  where


import qualified Control.Exception as E
import Data.Char (isSpace)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Directory as Dir
import System.Process (readProcess)



-- COMMIT COUNT
--
-- The number of commits in HEAD at compile time, e.g. "1234", used as a
-- build number. The splice depends on the git ref files, so committing or
-- switching branches recompiles the module using it. (If the ref has been
-- packed by `git gc` the file dependency degrades and the count can go
-- stale until the module recompiles for other reasons.) Building without
-- git or outside a checkout gives "unknown".


commitCount :: TH.Q TH.Exp
commitCount =
  do  addGitDependencies
      result <- TH.runIO (E.try (readProcess "git" ["rev-list", "--count", "HEAD"] ""))
      TH.lift $
        case (result :: Either E.SomeException String) of
          Right output -> filter (not . isSpace) output
          Left _ -> "unknown"


addGitDependencies :: TH.Q ()
addGitDependencies =
  do  addIfExists ".git/HEAD"
      contents <- TH.runIO (E.try (readFile ".git/HEAD"))
      case (contents :: Either E.SomeException String) of
        Right ('r':'e':'f':':':' ':ref) ->
          addIfExists (".git/" ++ filter (/= '\n') ref)

        _ ->
          return ()


addIfExists :: FilePath -> TH.Q ()
addIfExists path =
  do  exists <- TH.runIO (Dir.doesFileExist path)
      if exists
        then TH.addDependentFile path
        else return ()
