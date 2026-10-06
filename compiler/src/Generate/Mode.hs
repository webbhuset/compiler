module Generate.Mode
  ( Mode(..)
  , ShortNames(..)
  , isDebug
  , ShortFieldNames
  , shortenFieldNames
  , ShortCssNames
  , ShortChunkExports
  , withChunkExports
  , chunkExport
  , Callees
  , Callee(..)
  , callees
  , callee
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Optimized as Opt
import qualified AST.Prim.Name as N
import qualified Data.Index as Index
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.ModuleName as ModuleName
import qualified Generate.JavaScript.Name as JsName



-- MODE


-- Besides dev/prod, a mode carries what the generator needs to know about
-- the whole program while emitting one expression: the callees it may call
-- directly, and in prod the short names it may use.
data Mode
  = Dev Callees (Maybe Extract.Types)
  | Prod Callees ShortNames


data ShortNames =
  ShortNames
    { _fields :: ShortFieldNames
    , _cssNames :: ShortCssNames
    , _chunkExports :: ShortChunkExports
    }


-- The short names emitted for CSS classes, custom properties, and
-- keyframes, keyed by home module and record field name. Built by
-- Generate.Css.shortenNames.
type ShortCssNames =
  Map.Map (ModuleName.Canonical, N.Name) N.Name


-- The key each value a chunk exports is filed under in the chunk's export
-- record, and read back by wherever the program reaches it. Keys are only
-- unique within one chunk. Empty until the chunks are planned, which is
-- after the mode is made; see Generate.Chunks.exportKeys.
type ShortChunkExports =
  Map.Map Opt.Global JsName.Name


withChunkExports :: ShortChunkExports -> Mode -> Mode
withChunkExports keys mode =
  case mode of
    Dev _ _ ->
      mode

    Prod table (ShortNames fields cssNames _) ->
      Prod table (ShortNames fields cssNames keys)


-- In dev the key is the value's own global name, so the export records
-- stay readable.
chunkExport :: Mode -> Opt.Global -> JsName.Name
chunkExport mode global@(Opt.Global home name) =
  let
    own = JsName.fromGlobal home name
  in
  case mode of
    Dev _ _ ->
      own

    Prod _ shortNames ->
      Map.findWithDefault own global (_chunkExports shortNames)


isDebug :: Mode -> Bool
isDebug mode =
  case mode of
    Dev _ mi -> Maybe.isJust mi
    Prod _ _ -> False



-- CALLEES
--
-- A top-level function is emitted twice: once as the bare JavaScript
-- function of its arity, and once wrapped in F2..F9 so that partial
-- application and higher-order use keep working. A call that supplies at
-- least as many arguments as the function has parameters can then skip
-- the A2..A9 helper and call the bare function directly. A saturated
-- constructor application does not even need the call: it is the object
-- literal itself.
--
-- This table says, for every global that can be called that way, what
-- it is. It is built once from the whole graph, since the arity of a
-- definition in another package is only known from its optimized body.


type Callees =
  Map.Map Opt.Global Callee


data Callee
  = Function Int                 -- a function of this many parameters (1..9)
  | Ctor Index.ZeroBased Int     -- a constructor with this many fields (1+)
  | Tag Int                      -- a structural variant tag with this many fields (1+)


callee :: Mode -> Opt.Global -> Maybe Callee
callee mode global =
  case mode of
    Dev table _  -> Map.lookup global table
    Prod table _ -> Map.lookup global table


callees :: Map.Map Opt.Global Opt.Node -> Callees
callees nodes =
  Map.foldrWithKey addNode Map.empty nodes


addNode :: Opt.Global -> Opt.Node -> Callees -> Callees
addNode global@(Opt.Global home _) node table =
  case node of
    Opt.Define (Opt.Function args _) _ ->
      addFunction global (length args) table

    Opt.DefineTailFunc args _ _ ->
      addFunction global (length args) table

    Opt.Ctor index arity | arity > 0 ->
      Map.insert global (Ctor index arity) table

    Opt.Tag arity | arity > 0 ->
      Map.insert global (Tag arity) table

    Opt.Cycle _ _ defs _ ->
      List.foldl' (addCycleDef home) table defs

    _ ->
      table


addCycleDef :: ModuleName.Canonical -> Callees -> Opt.Def -> Callees
addCycleDef home table def =
  case def of
    Opt.Def name (Opt.Function args _) -> addFunction (Opt.Global home name) (length args) table
    Opt.Def _ _                        -> table
    Opt.TailDef name args _            -> addFunction (Opt.Global home name) (length args) table


-- A function of one parameter is already a bare JavaScript function, so
-- its direct name is its own; one of ten or more parameters has no F/A
-- helper and is emitted as a chain of one-parameter functions instead.
addFunction :: Opt.Global -> Int -> Callees -> Callees
addFunction global arity table =
  if 1 <= arity && arity <= 9 then
    Map.insert global (Function arity) table
  else
    table



-- SHORTEN FIELD NAMES


type ShortFieldNames =
  Map.Map N.Name JsName.Name


shortenFieldNames :: Opt.GlobalGraph -> ShortFieldNames
shortenFieldNames (Opt.GlobalGraph _ frequencies) =
  Map.foldr addToShortNames Map.empty $
    Map.foldrWithKey addToBuckets Map.empty frequencies


addToBuckets :: N.Name -> Int -> Map.Map Int [N.Name] -> Map.Map Int [N.Name]
addToBuckets field frequency buckets =
  Map.insertWith (++) frequency [field] buckets


addToShortNames :: [N.Name] -> ShortFieldNames -> ShortFieldNames
addToShortNames fields shortNames =
  List.foldl' addField shortNames fields


addField :: ShortFieldNames -> N.Name -> ShortFieldNames
addField shortNames field =
  let rename = JsName.fromInt (Map.size shortNames) in
  Map.insert field rename shortNames
