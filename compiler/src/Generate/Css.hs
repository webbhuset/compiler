{-# LANGUAGE OverloadedStrings #-}
module Generate.Css
  ( generate
  , shortenNames
  , classNameBuilder
  , varNameBuilder
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set

import qualified AST.Optimized as Opt
import qualified AST.Utils.Css as Css
import qualified Elm.ModuleName as ModuleName
import qualified Generate.Mode as Mode



-- GENERATE
--
-- Collect every [css| ... |] block that is reachable from the given mains
-- and render them as one stylesheet. In dev, names are emitted
-- module-qualified, e.g. class `card` in module Page.Checkout becomes
-- `Page-Checkout--card`; in --optimize they are shortened via the table in
-- the mode. Either way they match what Generate.JavaScript.Expression
-- emits for the block object.


generate :: Mode.Mode -> Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> [Opt.Global] -> Maybe B.Builder
generate mode (Opt.GlobalGraph nodes _) mains extraRoots =
  let
    seen =
      List.foldl' (addGlobal nodes)
        (Map.foldlWithKey'
          (\set home _ -> addGlobal nodes set (Opt.Global home "main"))
          Set.empty
          mains)
        extraRoots

    blocks =
      Map.foldr mainBlocks
        (concatMap (\g -> maybe [] nodeBlocks (Map.lookup g nodes)) (Set.toList seen))
        mains
  in
  case blocks of
    [] ->
      Nothing

    _ ->
      Just (mconcat (map (render mode) blocks))



-- NAMES


classNameBuilder :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> B.Builder
classNameBuilder mode home name =
  case mode of
    Mode.Dev _ ->
      homeToBuilder home <> "--" <> Name.toBuilder name

    Mode.Prod shortNames ->
      Name.toBuilder (Mode._cssNames shortNames ! (home, name))


varNameBuilder :: Mode.Mode -> ModuleName.Canonical -> Name.Name -> B.Builder
varNameBuilder mode home name =
  "--" <> classNameBuilder mode home name


homeToBuilder :: ModuleName.Canonical -> B.Builder
homeToBuilder (ModuleName.Canonical _ home) =
  B.stringUtf8 (map dotToDash (Name.toChars home))


dotToDash :: Char -> Char
dotToDash c =
  if c == '.' then '-' else c



-- SHORTEN NAMES
--
-- Assign every (home module, name) pair in the program a short CSS name,
-- walking all blocks in a deterministic order. Classes, keyframes, and
-- custom properties share one table; a class and a custom property with
-- the same name in one module share a short name, which is fine since
-- custom properties are always emitted with a `--` prefix.


shortenNames :: Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> Mode.ShortCssNames
shortenNames (Opt.GlobalGraph nodes _) mains =
  let
    blocks =
      Map.foldr mainBlocks (concatMap nodeBlocks (Map.elems nodes)) mains

    blockKeys (home, Css.Content _ (Css.Types classes keyframes vars)) =
      map ((,) home) (Set.toAscList classes ++ Set.toAscList keyframes ++ Map.keys vars)

    addKey (stream, table) key =
      if Map.member key table then
        (stream, table)
      else
        case stream of
          short : rest -> (rest, Map.insert key (Name.fromChars short) table)
          [] -> error "cssShortNameStream is infinite"
  in
  snd (List.foldl' addKey (cssShortNameStream, Map.empty) (concatMap blockKeys blocks))


cssShortNameStream :: [[Char]]
cssShortNameStream =
  let
    firstChars = ['a'..'z'] ++ ['A'..'Z']
    restChars = firstChars ++ ['0'..'9'] ++ ['_']

    combos n =
      if n <= 0
        then [[]]
        else [ c : rest | c <- restChars, rest <- combos (n - 1 :: Int) ]

    names =
      concatMap (\n -> [ c : rest | c <- firstChars, rest <- combos (n - 1) ]) [1 ..]
  in
  filter (\name -> Set.notMember (map Char.toLower name) cssReservedShortNames) names


-- CSS keywords are matched case-insensitively, so a generated @keyframes
-- name must never spell an animation keyword or the `animation` shorthand
-- would misparse. Same set as the declaration check in Parse.Css.
cssReservedShortNames :: Set.Set [Char]
cssReservedShortNames =
  Set.fromList
    [ "initial", "inherit", "unset", "revert", "default"
    , "none", "auto"
    , "normal", "reverse", "alternate"
    , "forwards", "backwards", "both"
    , "running", "paused"
    , "infinite"
    , "linear", "ease"
    ]



-- RENDER


render :: Mode.Mode -> (ModuleName.Canonical, Css.Content) -> B.Builder
render mode (home@(ModuleName.Canonical _ moduleName), Css.Content chunks _) =
  let
    comment =
      case mode of
        Mode.Dev _ -> "/* " <> Name.toBuilder moduleName <> " */"
        Mode.Prod _ -> mempty
  in
  comment
  <> foldMap (renderChunk mode home) chunks
  <> "\n"


renderChunk :: Mode.Mode -> ModuleName.Canonical -> Css.Chunk -> B.Builder
renderChunk mode home chunk =
  case chunk of
    Css.Text text        -> B.byteString text
    Css.ClassRef name    -> classNameBuilder mode home name
    Css.KeyframesRef name -> classNameBuilder mode home name
    Css.VarRef name      -> varNameBuilder mode home name


mainBlocks :: Opt.Main -> [(ModuleName.Canonical, Css.Content)] -> [(ModuleName.Canonical, Css.Content)]
mainBlocks main blocks =
  case main of
    Opt.Static -> blocks
    Opt.Dynamic _ decoder -> addExpr decoder blocks



-- REACHABILITY


addGlobal :: Map.Map Opt.Global Opt.Node -> Set.Set Opt.Global -> Opt.Global -> Set.Set Opt.Global
addGlobal nodes seen global =
  if Set.member global seen then
    seen
  else
    let
      seen' = Set.insert global seen
    in
    case Map.lookup global nodes of
      Nothing -> seen'
      Just node -> Set.foldl' (addGlobal nodes) seen' (nodeDeps node)


nodeDeps :: Opt.Node -> Set.Set Opt.Global
nodeDeps node =
  case node of
    Opt.Define _ deps          -> deps
    Opt.DefineTailFunc _ _ deps -> deps
    Opt.Ctor _ _               -> Set.empty
    Opt.Enum _                 -> Set.empty
    Opt.Box                    -> Set.empty
    Opt.Link global            -> Set.singleton global
    Opt.Cycle _ _ _ deps       -> deps
    Opt.Manager _              -> Set.empty
    Opt.Kernel _ deps          -> deps
    Opt.PortIncoming _ deps    -> deps
    Opt.PortOutgoing _ deps    -> deps
    Opt.PortTask _ _ deps      -> deps



-- COLLECT BLOCKS


nodeBlocks :: Opt.Node -> [(ModuleName.Canonical, Css.Content)]
nodeBlocks node =
  case node of
    Opt.Define expr _ ->
      addExpr expr []

    Opt.DefineTailFunc _ expr _ ->
      addExpr expr []

    Opt.Cycle _ pairs defs _ ->
      foldr (addExpr . snd) (foldr addDef [] defs) pairs

    Opt.PortIncoming expr _ ->
      addExpr expr []

    Opt.PortOutgoing expr _ ->
      addExpr expr []

    Opt.PortTask e1 e2 _ ->
      addExpr e1 (addExpr e2 [])

    _ ->
      []


addDef :: Opt.Def -> [(ModuleName.Canonical, Css.Content)] -> [(ModuleName.Canonical, Css.Content)]
addDef def blocks =
  case def of
    Opt.Def _ expr -> addExpr expr blocks
    Opt.TailDef _ _ expr -> addExpr expr blocks


addExpr :: Opt.Expr -> [(ModuleName.Canonical, Css.Content)] -> [(ModuleName.Canonical, Css.Content)]
addExpr expression blocks =
  case expression of
    Opt.Css home content ->
      (home, content) : blocks

    Opt.Bool _ -> blocks
    Opt.Chr _ -> blocks
    Opt.Str _ -> blocks
    Opt.Int _ -> blocks
    Opt.Float _ -> blocks
    Opt.VarLocal _ -> blocks
    Opt.VarGlobal _ -> blocks
    Opt.VarEnum _ _ -> blocks
    Opt.VarBox _ -> blocks
    Opt.VarCycle _ _ -> blocks
    Opt.VarDebug _ _ _ _ -> blocks
    Opt.VarKernel _ _ -> blocks
    Opt.List exprs -> foldr addExpr blocks exprs
    Opt.Function _ body -> addExpr body blocks
    Opt.Call func args -> addExpr func (foldr addExpr blocks args)
    Opt.TailCall _ args -> foldr (addExpr . snd) blocks args
    Opt.If branches final ->
      foldr (\(a, b) bs -> addExpr a (addExpr b bs)) (addExpr final blocks) branches
    Opt.Let def body -> addDef def (addExpr body blocks)
    Opt.Destruct _ body -> addExpr body blocks
    Opt.Case _ _ decider jumps ->
      addDecider decider (foldr (addExpr . snd) blocks jumps)
    Opt.Accessor _ -> blocks
    Opt.Access record _ -> addExpr record blocks
    Opt.Update record fields -> addExpr record (Map.foldr addExpr blocks fields)
    Opt.Record fields -> Map.foldr addExpr blocks fields
    Opt.Unit -> blocks
    Opt.Tuple a b maybeC ->
      addExpr a (addExpr b (foldr addExpr blocks maybeC))
    Opt.Shader _ _ _ -> blocks
    Opt.WorkerRef _ -> blocks


addDecider :: Opt.Decider Opt.Choice -> [(ModuleName.Canonical, Css.Content)] -> [(ModuleName.Canonical, Css.Content)]
addDecider decider blocks =
  case decider of
    Opt.Leaf choice ->
      case choice of
        Opt.Inline expr -> addExpr expr blocks
        Opt.Jump _ -> blocks

    Opt.Chain _ success failure ->
      addDecider success (addDecider failure blocks)

    Opt.FanOut _ tests fallback ->
      foldr (addDecider . snd) (addDecider fallback blocks) tests
