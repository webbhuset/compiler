{-# LANGUAGE OverloadedStrings #-}
module Generate.SourceMap
  ( Origin(..)
  , origin
  , marker
  , endMarker
  , strip
  , Source(..)
  , Target(..)
  , encode
  )
  where


import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg



-- SOURCE MAPS
--
-- `elm make --sourcemap` maps every line of the JavaScript a top level
-- definition compiles to back to that definition, and every line of kernel
-- code to its line in the kernel file. That is enough for a profiler or a
-- stack trace to name the Elm definition, which is what they are used for.
--
-- The generator does not track output positions. Instead, with source maps
-- on, it puts a marker line in front of each definition's code, naming it
-- by its index in the global graph.
-- Once a file is rendered the markers are taken out again, before names are
-- substituted and files hashed, so the JavaScript comes out byte for byte
-- as without --sourcemap, and the line each marker stood on is where its
-- definition starts.
--
-- The marker has a line of its own because Generate.JavaScript.Scope reads
-- top level definitions from column zero. Its payload is a number so that
-- nothing in it looks like an identifier to the same scan, and a short one
-- because a big program has thousands of markers to render and take out.



-- ORIGIN


-- A top level definition, by package, module, and name.
data Origin =
  Origin
    { _package :: String
    , _module :: String
    , _name :: String
    }
    deriving (Eq, Ord)


origin :: Opt.Global -> Origin
origin (Opt.Global (ModuleName.Canonical pkg home) name) =
  Origin (Pkg.toChars pkg) (Module.toChars home) (N.toChars name)


-- The definition at this index of the global graph's nodes.
marker :: Int -> B.Builder
marker index =
  B.byteString markerStart <> B.intDec index <> "\0\n"


-- Code after this is not any definition's: the runtime around the
-- definitions, or glue like effect manager registrations.
endMarker :: B.Builder
endMarker =
  B.byteString markerStart <> "\0\n"


markerStart :: BS.ByteString
markerStart =
  "\0elm-src\0"



-- STRIP


-- Take the markers out, and say which zero-based line each one stood on in
-- what is left, and the index it named. Nothing is an end marker.
strip :: BS.ByteString -> (BS.ByteString, [(Int, Maybe Int)])
strip bytes =
  let
    go rest line revChunks revFound =
      case BS.breakSubstring markerStart rest of
        (before, after)
          | BS.null after ->
              (BS.concat (reverse (before : revChunks)), reverse revFound)

          | otherwise ->
              let
                lineHere = line + BSC.count '\n' before
                body = BS.drop (BS.length markerStart) after
                (payload, afterPayload) = BS.break (== 0) body
                rest' = BS.drop 2 afterPayload -- the closing NUL and the newline
                found = (lineHere, decode payload) : revFound
              in
              go rest' lineHere (before : revChunks) found
  in
  go bytes 0 [] []


decode :: BS.ByteString -> Maybe Int
decode payload =
  case BSC.readInt payload of
    Just (index, rest) | BS.null rest -> Just index
    _                                 -> Nothing



-- ENCODE


-- A file a map points into: what DevTools calls it, and its text, which is
-- embedded so nothing has to be served for the map to work.
data Source =
  Source
    { _label :: String
    , _content :: BS.ByteString
    }


-- Where a definition's code goes back to: a source, and the zero-based line
-- its code starts on. Kernel code keeps its lines, so it is followed line by
-- line; Elm code compiles to more lines than it was written in, and every
-- one of them goes to the start of its definition, and the functions it
-- defines are named after it, qualified by its module.
data Target =
  Target
    { _source :: Int
    , _line :: Int
    , _lineByLine :: Bool
    , _home :: Maybe String
    }


-- A version 3 source map for a file, from the line each definition starts
-- on and where it came from. Nothing marks code whose origin is unknown,
-- which ends the mapping before it.
encode :: String -> [Source] -> BS.ByteString -> [(Int, Maybe Target)] -> BS.ByteString
encode file sources bytes starts =
  let
    table = Map.fromList starts

    -- the definition each line belongs to, and how far into it the line is
    owner line =
      case Map.lookupLE line table of
        Just (start, target) -> fmap (\t -> (t, line - start)) target
        Nothing              -> Nothing

    segmentLine (State prevSource prevLine prevName names revLines) (line, text, previousText) =
      case owner line of
        Nothing ->
          State prevSource prevLine prevName names ((if Map.member line table then "A" else "") : revLines)

        Just (Target source srcLine byLine home, offset) ->
          let
            l = if byLine then srcLine + offset else srcLine
            segment = vlq 0 <> vlq (source - prevSource) <> vlq (l - prevLine) <> vlq 0
          in
          case (,) <$> home <*> functionStart previousText text of
            Nothing ->
              State source l prevName names (segment : revLines)

            Just (moduleName, (column, name)) ->
              let
                qualified = moduleName ++ "." ++ name
                (index, names') =
                  case Map.lookup qualified (_indices names) of
                    Just i  -> (i, names)
                    Nothing -> (Map.size (_indices names), Names (Map.insert qualified (Map.size (_indices names)) (_indices names)) (qualified : _revNames names))
                -- the name covers only the paren, so that functions further
                -- along the line are not taken for this one
                named = "," <> vlq column <> "AAA" <> vlq (index - prevName) <> "," <> vlq 1 <> "AAA"
              in
              State source l index names' ((segment <> named) : revLines)

    lines' = BSC.lines bytes
    numbered = zip3 [0 ..] lines' (BS.empty : lines')
    State _ _ _ finalNames revMappings = List.foldl' segmentLine (State 0 0 0 (Names Map.empty []) []) numbered
    mappings = mconcat (List.intersperse ";" (reverse revMappings))
  in
  LBS.toStrict $ B.toLazyByteString $
    "{\"version\":3,\"file\":" <> string file
    <> ",\"sources\":[" <> commaSep (map (string . _label) sources) <> "]"
    <> ",\"sourcesContent\":[" <> commaSep (map (bytesString . _content) sources) <> "]"
    <> ",\"names\":[" <> commaSep (map string (reverse (_revNames finalNames))) <> "]"
    <> ",\"mappings\":\"" <> mappings <> "\"}\n"


data State =
  State
    { _prevSource :: !Int
    , _prevLine :: !Int
    , _prevName :: !Int
    , _names :: !Names
    , _revLines :: [B.Builder]
    }


data Names =
  Names
    { _indices :: !(Map.Map String Int)
    , _revNames :: [String]
    }



-- FUNCTION NAMES
--
-- DevTools names a function in a profile by the source map entry on the
-- paren its parameters open with, so each top level function gets one
-- there. They are written two ways:
--
--     var $author$project$Main$fn$update = function (model, msg) {
--
--     var $elm$core$Maybe$Just = F2(
--     	function (a, b) {
--
-- The name is what the variable ends in, so a direct call's `$fn$` and the
-- members of a recursive group get theirs too.


functionStart :: BS.ByteString -> BS.ByteString -> Maybe (Int, String)
functionStart previousText text =
  case variable text of
    Just (name, rhs) ->
      do  column <- paren (dropWrapper rhs)
          return (column, name)

    Nothing ->
      case variable previousText of
        Just (name, rhs) | isWrapperOpen rhs ->
          do  column <- paren (BSC.dropWhile (== '\t') text)
              return (column, name)

        _ ->
          Nothing
  where
    -- the column of the paren, in the line `rest` is the end of
    paren rest =
      if BS.isPrefixOf "function (" rest then Just (BS.length text - BS.length rest + 9) else Nothing

    dropWrapper rhs =
      if isWrapperOpen (BSC.takeWhile (/= '(') rhs <> "(")
        then BS.drop 1 (BSC.dropWhile (/= '(') rhs)
        else rhs


-- `var NAME = `, and what comes after it
variable :: BS.ByteString -> Maybe (String, BS.ByteString)
variable text =
  do  rest <- BS.stripPrefix "var " text
      let (identifier, afterIdentifier) = BSC.break (== ' ') rest
      rhs <- BS.stripPrefix " = " afterIdentifier
      let name = BSC.unpack (snd (BSC.breakEnd (== '$') identifier))
      if BSC.elem '$' identifier && not (null name) then Just (name, rhs) else Nothing


-- `F2(` to `F9(`, with nothing after it
isWrapperOpen :: BS.ByteString -> Bool
isWrapperOpen rhs =
  case BSC.unpack rhs of
    ['F', d, '('] -> d >= '2' && d <= '9'
    _             -> False



commaSep :: [B.Builder] -> B.Builder
commaSep =
  mconcat . List.intersperse ","


string :: String -> B.Builder
string s =
  "\"" <> B.stringUtf8 (concatMap escapeChar s) <> "\""


escapeChar :: Char -> String
escapeChar c =
  case c of
    '"'  -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ | Char.ord c < 0x20 -> "\\u" ++ pad (showHex4 (Char.ord c))
      | otherwise -> [c]
  where
    pad h = replicate (4 - length h) '0' ++ h
    showHex4 n = let digits = "0123456789abcdef" in if n < 16 then [digits !! n] else showHex4 (n `div` 16) ++ [digits !! (n `mod` 16)]


-- Like string, for UTF-8 text that is already bytes. Only ASCII needs
-- escaping, so the bytes are copied over in runs between the ones that do.
bytesString :: BS.ByteString -> B.Builder
bytesString bytes =
  let
    needsEscape w = w < 0x20 || w == 0x22 || w == 0x5C

    go rest =
      case BS.break needsEscape rest of
        (run, after) ->
          case BS.uncons after of
            Nothing        -> B.byteString run
            Just (w, more) -> B.byteString run <> B.stringUtf8 (escapeChar (Char.chr (fromIntegral w))) <> go more
  in
  "\"" <> go bytes <> "\""


-- Base64 VLQ, as source maps spell numbers.
vlq :: Int -> B.Builder
vlq n =
  let
    start = if n < 0 then (negate n `Bits.shiftL` 1) Bits..|. 1 else n `Bits.shiftL` 1
    go v =
      let
        digit = v Bits..&. 31
        rest = v `Bits.shiftR` 5
      in
      if rest == 0
        then B.char7 (base64 digit)
        else B.char7 (base64 (digit Bits..|. 32)) <> go rest
  in
  go start


base64 :: Int -> Char
base64 i =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !! i
