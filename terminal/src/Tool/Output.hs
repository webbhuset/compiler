module Tool.Output
  ( Output(..)
  )
  where


import qualified Json.Encode as E



-- OUTPUT
--
-- Every command produces both forms, and `--json` picks one.


data Output =
  Output
    { _text :: String
    , _json :: E.Value
    }
