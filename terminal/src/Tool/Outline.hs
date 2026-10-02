{-# LANGUAGE OverloadedStrings #-}
module Tool.Outline
  ( run
  )
  where


import qualified Json.Encode as E
import Json.Encode ((==>))
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import Tool.Summary (Summary(..), Entry(..))
import qualified Tool.Summary as S



-- RUN
--
--   elm tool outline Some.Module
--
-- One line per declaration, in source order, with the lines it covers:
--
--   src/Page/Home.elm
--   module Page.Home exposing (Model, Msg(..), update)
--   12-15    type Msg = Increment | Decrement Int
--   17-34    update : Msg -> Model -> Model
--   36-38    helper : Int -> Int  [private]


run :: String -> IO (Either Problem Output)
run target =
  case Project.parseModuleName target of
    Nothing ->
      return (Left (BadName "a module like `Page.Home`" target))

    Just home ->
      Project.withModule False home $ \summary ->
        return $ Right $ Output (toText summary) (toJson summary)


toText :: Summary -> String
toText (Summary home location exposing _ entries) =
  let
    line e =
      maybe "" (\(start, end) -> pad 9 (show start ++ "-" ++ show end)) (_lines e)
      ++ _line e
      ++ (if _exposed e then "" else "  [private]")
      ++ "\n"
  in
  location ++ "\n" ++ "module " ++ home ++ " exposing " ++ exposing ++ "\n" ++ concatMap line entries


pad :: Int -> String -> String
pad n s =
  s ++ replicate (max 1 (n - length s)) ' '


toJson :: Summary -> E.Value
toJson (Summary home location _ _ entries) =
  let
    entryToJson e =
      E.object $
        [ "name" ==> E.chars (_name e)
        , "kind" ==> E.chars (S.kindToChars (_kind e))
        , "signature" ==> E.chars (_line e)
        , "exposed" ==> E.bool (_exposed e)
        ]
        ++
          case _lines e of
            Just (start, end) -> [ "start" ==> E.int start, "end" ==> E.int end ]
            Nothing           -> []
  in
  E.object
    [ "module" ==> E.chars home
    , "location" ==> E.chars location
    , "declarations" ==> E.list entryToJson entries
    ]
