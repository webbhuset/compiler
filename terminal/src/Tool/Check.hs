{-# LANGUAGE OverloadedStrings #-}
module Tool.Check
  ( run
  )
  where


import qualified Json.Encode as E
import Json.Encode ((==>))
import Tool.Output (Output(..))
import Tool.Problem (Problem)
import qualified Tool.Project as Project



-- RUN
--
--   elm tool check                     every module in the source directories
--   elm tool check src/Main.elm ...    those files and what they import
--
-- Errors are reported like `elm make` reports them, and --json reports them
-- like `elm make --report=json`.


run :: [FilePath] -> IO (Either Problem Output)
run paths =
  do  result <- Project.check paths
      return $ flip fmap result $ \count ->
        Output
          ("No errors in " ++ show count ++ (if count == 1 then " module" else " modules") ++ ".\n")
          (E.object [ "errors" ==> E.list id [], "checked" ==> E.int count ])
