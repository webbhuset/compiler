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
--
-- With one exception: in dev mode the values of a recursive group are
-- defined inside a `try` block, so that the error for a value that needs
-- itself to exist can name the cycle. A `var` belongs to the enclosing
-- function, not the block, so those are top-level bindings all the same,
-- one tab in. Missing them left a chunk that used one of those values
-- with nothing to bind it to. Only a `var`: a module is strict code, where
-- a function declared in a block is scoped to the block.


topLevelNames :: BS.ByteString -> Set.Set BS.ByteString
topLevelNames bytes =
  fst (List.foldl' addLine (Set.empty, False) (BSC.lines bytes))


-- The flag says whether the line is inside a top-level `try` block.
addLine :: (Set.Set BS.ByteString, Bool) -> BS.ByteString -> (Set.Set BS.ByteString, Bool)
addLine (names, inTry) line
  | line == "try {"                 = (names, True)
  | BS.isPrefixOf "} catch (" line  = (names, False)
  | inTry                           = (maybe names (addDefinition names) (BS.stripPrefix "\tvar " line >> BS.stripPrefix "\t" line), True)
  | otherwise                       = (addDefinition names line, False)


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
