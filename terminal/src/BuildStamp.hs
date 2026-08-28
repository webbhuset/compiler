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
-- build number. The splice depends on the HEAD reflog, which git appends
-- to on every commit, checkout, merge, and reset — so any of those
-- recompiles the module using the splice. (Loose ref files would not work:
-- repos on the reftable backend have none.) Building without git or
-- outside a checkout gives "unknown".


commitCount :: TH.Q TH.Exp
commitCount =
  do  addIfExists ".git/HEAD"
      addIfExists ".git/logs/HEAD"
      result <- TH.runIO (E.try (readProcess "git" ["rev-list", "--count", "HEAD"] ""))
      TH.lift $
        case (result :: Either E.SomeException String) of
          Right output -> filter (not . isSpace) output
          Left _ -> "unknown"


addIfExists :: FilePath -> TH.Q ()
addIfExists path =
  do  exists <- TH.runIO (Dir.doesFileExist path)
      if exists
        then TH.addDependentFile path
        else return ()
