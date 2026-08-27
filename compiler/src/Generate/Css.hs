{-# LANGUAGE OverloadedStrings #-}
module Generate.Css
  ( generate
  , classNameBuilder
  , varNameBuilder
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set

import qualified AST.Optimized as Opt
import qualified AST.Utils.Css as Css
import qualified Elm.ModuleName as ModuleName



-- GENERATE
--
-- Collect every [css| ... |] block that is reachable from the given mains
-- and render them as one stylesheet. Names are emitted module-qualified,
-- e.g. class `card` in module Page.Checkout becomes `Page-Checkout--card`,
-- matching what Generate.JavaScript.Expression emits for the block object.


generate :: Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> Maybe B.Builder
generate (Opt.GlobalGraph nodes _) mains =
  let
    seen =
      Map.foldlWithKey'
        (\set home _ -> addGlobal nodes set (Opt.Global home "main"))
        Set.empty
        mains

    fromMain main bs =
      case main of
        Opt.Static -> bs
        Opt.Dynamic _ decoder -> addExpr decoder bs

    blocks =
      Map.foldr fromMain
        (concatMap (\g -> maybe [] nodeBlocks (Map.lookup g nodes)) (Set.toList seen))
        mains
  in
  case blocks of
    [] ->
      Nothing

    _ ->
      Just (mconcat (map render blocks))



-- NAMES


classNameBuilder :: ModuleName.Canonical -> Name.Name -> B.Builder
classNameBuilder home name =
  homeToBuilder home <> "--" <> Name.toBuilder name


varNameBuilder :: ModuleName.Canonical -> Name.Name -> B.Builder
varNameBuilder home name =
  "--" <> homeToBuilder home <> "--" <> Name.toBuilder name


homeToBuilder :: ModuleName.Canonical -> B.Builder
homeToBuilder (ModuleName.Canonical _ home) =
  B.stringUtf8 (map dotToDash (Name.toChars home))


dotToDash :: Char -> Char
dotToDash c =
  if c == '.' then '-' else c



-- RENDER


render :: (ModuleName.Canonical, Css.Content) -> B.Builder
render (home@(ModuleName.Canonical _ moduleName), Css.Content chunks _) =
  "/* " <> Name.toBuilder moduleName <> " */"
  <> foldMap (renderChunk home) chunks
  <> "\n"


renderChunk :: ModuleName.Canonical -> Css.Chunk -> B.Builder
renderChunk home chunk =
  case chunk of
    Css.Text text        -> B.byteString text
    Css.ClassRef name    -> classNameBuilder home name
    Css.KeyframesRef name -> classNameBuilder home name
    Css.VarRef name      -> varNameBuilder home name



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
