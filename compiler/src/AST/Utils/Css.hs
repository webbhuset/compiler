module AST.Utils.Css
  ( Content(..)
  , Chunk(..)
  , Types(..)
  , PropType(..)
  )
  where


import Control.Monad (liftM, liftM2, liftM3)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set



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
  | ClassRef Name.Name
  | VarRef Name.Name
  | KeyframesRef Name.Name



-- TYPES
--
-- _classes and _keyframes together become the first record parameter of
-- Css.Stylesheet. _vars holds only the inputs: custom properties that the
-- block consumes but never assigns, which Elm must supply via Css.vars.


data Types =
  Types
    { _classes :: Set.Set Name.Name
    , _keyframes :: Set.Set Name.Name
    , _vars :: Map.Map Name.Name PropType
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


instance Binary Content where
  get = liftM2 Content get get
  put (Content a b) = put a >> put b


instance Binary Chunk where
  put chunk =
    case chunk of
      Text a         -> putWord8 0 >> put a
      ClassRef a     -> putWord8 1 >> put a
      VarRef a       -> putWord8 2 >> put a
      KeyframesRef a -> putWord8 3 >> put a

  get =
    do  word <- getWord8
        case word of
          0 -> liftM Text get
          1 -> liftM ClassRef get
          2 -> liftM VarRef get
          3 -> liftM KeyframesRef get
          _ -> fail "problem getting Css.Chunk binary"


instance Binary Types where
  get = liftM3 Types get get get
  put (Types a b c) = put a >> put b >> put c


instance Binary PropType where
  put propType =
    putWord8 $
      case propType of
        Value      -> 0
        Length     -> 1
        Percentage -> 2
        Color      -> 3
        Number     -> 4
        Integer    -> 5
        Duration   -> 6
        Angle      -> 7

  get =
    do  word <- getWord8
        case word of
          0 -> return Value
          1 -> return Length
          2 -> return Percentage
          3 -> return Color
          4 -> return Number
          5 -> return Integer
          6 -> return Duration
          7 -> return Angle
          _ -> fail "problem getting Css.PropType binary"
