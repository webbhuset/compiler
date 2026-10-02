{-# LANGUAGE OverloadedStrings #-}
module Generate.SourceMap
  ( Origin(..)
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
import qualified Data.ByteString.UTF8 as BS_UTF8
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
-- on, it puts a marker line in front of each definition's code, naming it.
-- Once a file is rendered the markers are taken out again, before names are
-- substituted and files hashed, so the JavaScript comes out byte for byte
-- as without --sourcemap, and the line each marker stood on is where its
-- definition starts.
--
-- The marker has a line of its own because Generate.JavaScript.Scope reads
-- top level definitions from column zero. Its payload is spelled in digits
-- so that nothing in it looks like an identifier to the same scan.



-- ORIGIN


-- A top level definition, by package, module, and name.
data Origin =
  Origin
    { _package :: String
    , _module :: String
    , _name :: String
    }
    deriving (Eq, Ord)


marker :: Opt.Global -> B.Builder
marker (Opt.Global (ModuleName.Canonical pkg home) name) =
  let
    payload = Pkg.toChars pkg ++ "\0" ++ Module.toChars home ++ "\0" ++ N.toChars name
    digits = List.intercalate "." (map (show . fromEnum) (BS.unpack (BS_UTF8.fromString payload)))
  in
  B.byteString markerStart <> B.stringUtf8 digits <> "\0\n"


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
-- what is left. Nothing is an end marker.
strip :: BS.ByteString -> (BS.ByteString, [(Int, Maybe Origin)])
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


decode :: BS.ByteString -> Maybe Origin
decode payload =
  case mapM (readByte . BSC.unpack) (BSC.split '.' payload) of
    Just byteValues ->
      case splitOn '\0' (BS_UTF8.toString (BS.pack byteValues)) of
        [pkg, home, name] -> Just (Origin pkg home name)
        _                 -> Nothing

    Nothing ->
      Nothing
  where
    readByte s =
      if not (null s) && all Char.isDigit s && read s < (256 :: Int) then Just (fromIntegral (read s :: Int)) else Nothing


splitOn :: Char -> String -> [String]
splitOn sep text =
  case break (== sep) text of
    (chunk, [])     -> [chunk]
    (chunk, _:rest) -> chunk : splitOn sep rest



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
-- one of them goes to the start of its definition.
data Target =
  Target
    { _source :: Int
    , _line :: Int
    , _lineByLine :: Bool
    }


-- A version 3 source map for a file of the given number of lines, from the
-- line each definition starts on and where it came from. Nothing marks code
-- whose origin is unknown, which ends the mapping before it.
encode :: String -> [Source] -> Int -> [(Int, Maybe Target)] -> BS.ByteString
encode file sources lineCount starts =
  let
    table = Map.fromList starts

    -- the definition each line belongs to, and how far into it the line is
    owner line =
      case Map.lookupLE line table of
        Just (start, target) -> fmap (\t -> (t, line - start)) target
        Nothing              -> Nothing

    segmentLine (previous, revLines) line =
      case owner line of
        Nothing ->
          (previous, (if Map.member line table then "A" else "") : revLines)

        Just (Target source srcLine byLine, offset) ->
          let
            l = if byLine then srcLine + offset else srcLine
            (prevSource, prevLine) = previous
            segment = vlq 0 <> vlq (source - prevSource) <> vlq (l - prevLine) <> vlq 0
          in
          ((source, l), segment : revLines)

    (_, revMappings) = List.foldl' segmentLine ((0, 0), []) [0 .. lineCount - 1]
    mappings = mconcat (List.intersperse ";" (reverse revMappings))
  in
  LBS.toStrict $ B.toLazyByteString $
    "{\"version\":3,\"file\":" <> string file
    <> ",\"sources\":[" <> commaSep (map (string . _label) sources) <> "]"
    <> ",\"sourcesContent\":[" <> commaSep (map (string . BS_UTF8.toString . _content) sources) <> "]"
    <> ",\"names\":[],\"mappings\":\"" <> mappings <> "\"}\n"


commaSep :: [B.Builder] -> B.Builder
commaSep =
  mconcat . List.intersperse ","


string :: String -> B.Builder
string s =
  "\"" <> B.stringUtf8 (concatMap escape s) <> "\""
  where
    escape c =
      case c of
        '"'  -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ | Char.ord c < 0x20 -> "\\u" ++ pad (showHex4 (Char.ord c))
          | otherwise -> [c]
    pad h = replicate (4 - length h) '0' ++ h
    showHex4 n = let digits = "0123456789abcdef" in if n < 16 then [digits !! n] else showHex4 (n `div` 16) ++ [digits !! (n `mod` 16)]


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
