{-# LANGUAGE OverloadedStrings #-}
module Tool.Place
  ( Place(..)
  , fromRegion
  , withSource
  , toText
  , toJson
  , sortPlaces
  )
  where


import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import System.FilePath ((</>))

import qualified File
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import qualified Tool.Index as Index



-- PLACES
--
-- A spot in the source, printed like grep prints it, with the line it is on:
--
--   src/Main.elm:61:22: Home.update subMsg home


data Place =
  Place
    { _path :: FilePath
    , _line :: Int
    , _column :: Int
    , _endLine :: Int
    , _endColumn :: Int
    , _label :: String
    , _note :: String
      -- printed after the source line
    , _source :: String
    }


fromRegion :: FilePath -> String -> A.Region -> Place
fromRegion path label region =
  let
    (sr, sc) = Index.start region
    (er, ec) = Index.end region
  in
  Place path sr sc er ec label "" ""


-- Fill in the source lines, reading each file once.
withSource :: FilePath -> [Place] -> IO [Place]
withSource root places =
  do  let paths = List.nub (map _path places)
      files <- traverse (\p -> (,) p . lines . BS_UTF8.toString <$> File.readUtf8 (root </> p)) paths
      return
        [ p { _source = trim (Maybe.fromMaybe "" (lookup (_path p) files >>= \ls -> at ls (_line p - 1))) }
        | p <- places
        ]


at :: [a] -> Int -> Maybe a
at xs n =
  case drop n xs of
    x:_ | n >= 0 -> Just x
    _            -> Nothing


trim :: String -> String
trim =
  List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace


sortPlaces :: [Place] -> [Place]
sortPlaces =
  List.sortOn (\p -> (_path p, _line p, _column p))


toText :: Place -> String
toText p =
  _path p ++ ":" ++ show (_line p) ++ ":" ++ show (_column p) ++ ": " ++ _source p
  ++ (if null (_note p) then "" else "  -- " ++ _note p)
  ++ "\n"


toJson :: Place -> E.Value
toJson p =
  E.object $
    [ "path" ==> E.chars (_path p)
    , "line" ==> E.int (_line p)
    , "column" ==> E.int (_column p)
    , "endLine" ==> E.int (_endLine p)
    , "endColumn" ==> E.int (_endColumn p)
    , "kind" ==> E.chars (_label p)
    , "text" ==> E.chars (_source p)
    ]
    ++ (if null (_note p) then [] else [ "note" ==> E.chars (_note p) ])
