module Tool.Edit
  ( Edit(..)
  , apply
  , readLines
  , lastSegmentAt
  , leadingName
  )
  where


import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import System.FilePath ((</>))

import qualified File



-- EDITS
--
-- A replacement of part of one line, with lines and columns counting from
-- one and the end column just past the last character replaced.


data Edit =
  Edit
    { _path :: FilePath
    , _line :: Int
    , _column :: Int
    , _endColumn :: Int
    , _replacement :: String
    }
    deriving (Eq)


readLines :: FilePath -> FilePath -> IO [String]
readLines root path =
  lines . BS_UTF8.toString <$> File.readUtf8 (root </> path)


-- Apply the edits, file by file, from the end of each file backwards so the
-- earlier positions stay where they were.
apply :: FilePath -> [Edit] -> IO ()
apply root edits =
  let
    byFile = Map.fromListWith (++) [ (_path e, [e]) | e <- List.nub edits ]
  in
  mapM_ (applyToFile root) (Map.toList byFile)


applyToFile :: FilePath -> (FilePath, [Edit]) -> IO ()
applyToFile root (path, edits) =
  do  bytes <- File.readUtf8 (root </> path)
      let source = BS_UTF8.toString bytes
          ls = lines source
          sorted = List.sortOn (\e -> (negate (_line e), negate (_column e))) edits
          edited = foldl editLine ls sorted
          trailing = if not (null source) && last source == '\n' then "\n" else ""
      BS.writeFile (root </> path) (BS_UTF8.fromString (List.intercalate "\n" edited ++ trailing))


editLine :: [String] -> Edit -> [String]
editLine ls (Edit _ line col endCol replacement) =
  case splitAt (line - 1) ls of
    (before, l : after) ->
      before ++ [take (col - 1) l ++ replacement ++ drop (endCol - 1) l] ++ after

    _ ->
      ls



-- NAMES IN THE SOURCE


-- In a qualified name like `Page.Home.view` starting at a column, the
-- columns of its last part.
lastSegmentAt :: String -> Int -> Int -> (Int, Int)
lastSegmentAt line col endCol =
  let
    text = take (endCol - col) (drop (col - 1) line)
    lastDot = List.elemIndices '.' text
  in
  case lastDot of
    [] -> (col, endCol)
    _  -> (col + last lastDot + 1, endCol)


-- The qualified name a pattern like `Home.Loaded data` starts with.
leadingName :: String -> Int -> (Int, Int)
leadingName line col =
  let
    rest = drop (col - 1) line
    token = takeWhile (\c -> Char.isAlphaNum c || c == '_' || c == '.') rest
  in
  lastSegmentAt line col (col + length token)
