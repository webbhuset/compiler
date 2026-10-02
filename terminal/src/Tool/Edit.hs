module Tool.Edit
  ( Edit(..)
  , apply
  , toPlace
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
import Tool.Place (Place(Place))



-- EDITS
--
-- A replacement of part of one line, with lines and columns counting from
-- one and the end column just past the last character replaced. Span puts
-- the end on another line, and its replacement may hold newlines, which is
-- how lines are inserted and deleted.


data Edit
  = Edit
      { _path :: FilePath
      , _line :: Int
      , _column :: Int
      , _endColumn :: Int
      , _replacement :: String
      }
  | Span
      { _path :: FilePath
      , _line :: Int
      , _column :: Int
      , _endLine :: Int
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
editLine ls edit =
  case edit of
    Edit p line col endCol replacement ->
      editLine ls (Span p line col line endCol replacement)

    Span _ line col endLine endCol replacement ->
      let
        (before, rest) = splitAt (line - 1) ls
        first = case rest of l : _ -> l; [] -> ""
        lastLine = case drop (endLine - line) rest of l : _ -> l; [] -> ""
        after = drop (endLine - line + 1) rest
      in
      before ++ [take (col - 1) first ++ replacement ++ drop (endCol - 1) lastLine] ++ after



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



-- An edit as a place to print: where it is, and what goes there.
toPlace :: Edit -> Place
toPlace edit =
  case edit of
    Edit path line col endCol replacement ->
      Place path line col line endCol "edit" ("-> " ++ replacement) ""

    Span path line col endLine endCol replacement ->
      Place path line col endLine endCol "edit" (describe replacement) ""
  where
    describe r =
      case lines r of
        [] -> "delete"
        l : rest
          | all (all Char.isSpace) (l : rest) -> "delete"
          | otherwise -> "-> " ++ (if null l then "\\n" ++ concat (take 1 (filter (not . null) rest)) else l) ++ (if null rest then "" else " ...")
