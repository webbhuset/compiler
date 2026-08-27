module AST.Utils.Css
  ( Content(..)
  , Chunk(..)
  , Types(..)
  , PropType(..)
  --
  , eContent, dContent
  )
  where


import Control.Monad (liftM, liftM2, liftM3)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E

import qualified AST.Prim.Name as N



-- CONTENT
--
-- A parsed [css| ... |] block. The chunks reproduce the source text
-- exactly, except that class selectors, custom properties, and keyframes
-- names are held symbolically so they can be emitted with module-scoped
-- names. The types describe the record type of the block.


data Content =
  Content
    { _chunks :: [Chunk]
    , _types :: Types
    }


data Chunk
  = Text BS.ByteString
  | ClassRef N.Name
  | VarRef N.Name
  | KeyframesRef N.Name



-- TYPES
--
-- _classes and _keyframes together become the first record parameter of
-- Css.Stylesheet. _vars holds only the inputs: custom properties that the
-- block consumes but never assigns, which Elm must supply via Css.vars.


data Types =
  Types
    { _classes :: Set.Set N.Name
    , _keyframes :: Set.Set N.Name
    , _vars :: Map.Map N.Name PropType
    }


data PropType
  = Value
  | Length
  | Percentage
  | Color
  | Number
  | Integer
  | Duration
  | Angle



-- BINARY


eContent :: Content -> E.Builder
eContent (Content chunks types) =
  E.list32 eChunk chunks <> eTypes types


dContent :: D.Decoder Content
dContent =
  liftM2 Content (D.list32 dChunk) dTypes


eChunk :: Chunk -> E.Builder
eChunk chunk =
  case chunk of
    Text bytes     -> E.u8 0 <> E.byteString64 bytes
    ClassRef n     -> E.u8 1 <> N.encode n
    VarRef n       -> E.u8 2 <> N.encode n
    KeyframesRef n -> E.u8 3 <> N.encode n


dChunk :: D.Decoder Chunk
dChunk =
  do  tag <- D.u8
      case tag of
        0 -> liftM Text D.byteString64
        1 -> liftM ClassRef N.decode
        2 -> liftM VarRef N.decode
        3 -> liftM KeyframesRef N.decode
        _ -> D.expecting "Css.Chunk"


eTypes :: Types -> E.Builder
eTypes (Types classes keyframes vars) =
  E.set32 N.encode classes <> E.set32 N.encode keyframes <> E.dict32 N.encode ePropType vars


dTypes :: D.Decoder Types
dTypes =
  liftM3 Types (D.set32 N.decode) (D.set32 N.decode) (D.dict32 N.decode dPropType)


ePropType :: PropType -> E.Builder
ePropType propType =
  E.u8 $
    case propType of
      Value      -> 0
      Length     -> 1
      Percentage -> 2
      Color      -> 3
      Number     -> 4
      Integer    -> 5
      Duration   -> 6
      Angle      -> 7


dPropType :: D.Decoder PropType
dPropType =
  do  tag <- D.u8
      case tag of
        0 -> return Value
        1 -> return Length
        2 -> return Percentage
        3 -> return Color
        4 -> return Number
        5 -> return Integer
        6 -> return Duration
        7 -> return Angle
        _ -> D.expecting "Css.PropType"
