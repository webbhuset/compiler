{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Scope
  ( topLevelNames
  , mentionedNames
  )
  where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Word (Word8)



-- WHAT ONE BUNDLE DEFINES AND WHAT ANOTHER MENTIONS
--
-- A code-splitting chunk is a module of its own, so it cannot see the main
-- bundle's top-level bindings; it takes the ones it needs as an argument
-- and rebinds them locally. Working out which ones those are means asking
-- two questions of generated JavaScript, and asking them of the text is
-- the only way to reach the kernel: kernel code arrives as raw source, so
-- what it defines is not in the graph anywhere.
--
-- The two questions are deliberately asymmetric. A name is offered only if
-- it is *defined* at the top level of the main bundle, and taken only if
-- it is *mentioned* anywhere in the chunk. Mentions are allowed to
-- over-report -- a word inside a string literal costs one unused binding
-- and nothing else -- while definitions must not, which is why they are
-- anchored to column zero. Everything the compiler emits at the top level
-- of a bundle starts there, and everything nested is indented.


topLevelNames :: BS.ByteString -> Set.Set BS.ByteString
topLevelNames bytes =
  List.foldl' addDefinition Set.empty (BSC.lines bytes)


addDefinition :: Set.Set BS.ByteString -> BS.ByteString -> Set.Set BS.ByteString
addDefinition names line =
  case stripKeyword line of
    Nothing ->
      names

    Just rest ->
      let name = BS.takeWhile isIdentByte rest in
      if BS.null name || isDigitByte (BS.head name)
        then names
        else Set.insert name names


stripKeyword :: BS.ByteString -> Maybe BS.ByteString
stripKeyword line =
  if BS.isPrefixOf "var " line then
    Just (BS.drop 4 line)
  else if BS.isPrefixOf "function " line then
    Just (BS.drop 9 line)
  else
    Nothing


mentionedNames :: BS.ByteString -> Set.Set BS.ByteString
mentionedNames =
  go Set.empty
  where
    go names bytes =
      let rest = BS.dropWhile (not . isIdentByte) bytes in
      if BS.null rest then
        names
      else
        let (word, more) = BS.span isIdentByte rest in
        go (if isDigitByte (BS.head word) then names else Set.insert word names) more


{-# INLINE isIdentByte #-}
isIdentByte :: Word8 -> Bool
isIdentByte w =
  (w >= 0x61 && w <= 0x7A)    -- a-z
    || (w >= 0x41 && w <= 0x5A)  -- A-Z
    || (w >= 0x30 && w <= 0x39)  -- 0-9
    || w == 0x5F                 -- _
    || w == 0x24                 -- $


{-# INLINE isDigitByte #-}
isDigitByte :: Word8 -> Bool
isDigitByte w =
  w >= 0x30 && w <= 0x39
