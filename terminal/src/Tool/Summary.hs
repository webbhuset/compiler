module Tool.Summary
  ( Summary(..)
  , Entry(..)
  , Kind(..)
  , kindToChars
  , docsOrder
  , suggest
  )
  where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Reporting.Suggest as Suggest



-- SUMMARY
--
-- What the commands print, whether the module is part of the project, where
-- the type checker produced it, or comes from a package, where docs.json did.


data Summary =
  Summary
    { _module :: String
    , _location :: String
      -- a path for a project module, the package for a package module
    , _exposing :: String
    , _overview :: Maybe String
    , _entries :: [Entry]
      -- in source order when known, otherwise in `@docs` order
    }


data Entry =
  Entry
    { _name :: String
    , _kind :: Kind
    , _line :: String
      -- the declaration on one line, constructors included
    , _code :: String
      -- the declaration laid out as it would be written, as a reader of the
      -- docs gets to see it: an opaque type has no constructors
    , _comment :: Maybe String
    , _exposed :: Bool
    , _annotated :: Bool
    , _lines :: Maybe (Int, Int)
    }


data Kind
  = Value
  | Type
  | Alias
  | Tag
  | Binop


kindToChars :: Kind -> String
kindToChars kind =
  case kind of
    Value -> "value"
    Type  -> "type"
    Alias -> "alias"
    Tag   -> "tag"
    Binop -> "binop"



-- DOCS ORDER


-- Put entries in the order the `@docs` lines of a module comment name them,
-- leaving the rest where they were.
docsOrder :: Maybe String -> [Entry] -> [Entry]
docsOrder overview entries =
  let
    named = maybe [] docsNames overview
    rank = Map.fromList (zip named [(0::Int)..])
    position e = Map.findWithDefault maxBound (_name e) rank
  in
  List.sortOn position entries


docsNames :: String -> [String]
docsNames overview =
  [ stripParens (trim name)
  | line <- lines overview
  , Just rest <- [List.stripPrefix "@docs" (dropWhile Char.isSpace line)]
  , name <- splitOn ',' rest
  ]


trim :: String -> String
trim =
  List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace


stripParens :: String -> String
stripParens s =
  case s of
    '(' : inner | not (null inner) && last inner == ')' -> init inner
    _                                                   -> s


splitOn :: Char -> String -> [String]
splitOn sep string =
  case break (== sep) string of
    (chunk, [])     -> [chunk]
    (chunk, _:rest) -> chunk : splitOn sep rest



-- SUGGEST


suggest :: String -> [String] -> [String]
suggest target candidates =
  filter (\c -> sameKind c && Suggest.distance target c <= max 2 (length target `div` 3)) $
    Suggest.sort target id candidates
  where
    isWord s = case s of c:_ -> Char.isAlpha c; [] -> False
    sameKind c = isWord c == isWord target
