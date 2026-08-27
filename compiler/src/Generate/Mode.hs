module Generate.Mode
  ( Mode(..)
  , ShortNames(..)
  , isDebug
  , ShortFieldNames
  , shortenFieldNames
  , ShortCssNames
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name

import qualified AST.Optimized as Opt
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.ModuleName as ModuleName
import qualified Generate.JavaScript.Name as JsName



-- MODE


data Mode
  = Dev (Maybe Extract.Types)
  | Prod ShortNames


data ShortNames =
  ShortNames
    { _fields :: ShortFieldNames
    , _cssNames :: ShortCssNames
    }


-- The short names emitted for CSS classes, custom properties, and
-- keyframes, keyed by home module and record field name. Built by
-- Generate.Css.shortenNames.
type ShortCssNames =
  Map.Map (ModuleName.Canonical, Name.Name) Name.Name


isDebug :: Mode -> Bool
isDebug mode =
  case mode of
    Dev mi -> Maybe.isJust mi
    Prod _ -> False



-- SHORTEN FIELD NAMES


type ShortFieldNames =
  Map.Map Name.Name JsName.Name


shortenFieldNames :: Opt.GlobalGraph -> ShortFieldNames
shortenFieldNames (Opt.GlobalGraph _ frequencies) =
  Map.foldr addToShortNames Map.empty $
    Map.foldrWithKey addToBuckets Map.empty frequencies


addToBuckets :: Name.Name -> Int -> Map.Map Int [Name.Name] -> Map.Map Int [Name.Name]
addToBuckets field frequency buckets =
  Map.insertWith (++) frequency [field] buckets


addToShortNames :: [Name.Name] -> ShortFieldNames -> ShortFieldNames
addToShortNames fields shortNames =
  List.foldl' addField shortNames fields


addField :: ShortFieldNames -> Name.Name -> ShortFieldNames
addField shortNames field =
  let rename = JsName.fromInt (Map.size shortNames) in
  Map.insert field rename shortNames
