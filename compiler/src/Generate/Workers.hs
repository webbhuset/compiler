{-# LANGUAGE OverloadedStrings #-}
module Generate.Workers
  ( plan
  , token
  , tokenBuilder
  , globalToChars
  )
  where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg



-- PLAN
--
-- Find every worker program that a live Worker.spawn references, walking
-- through nested workers (a worker can spawn workers). The result is in
-- dependency order: a worker comes after every worker it spawns, so its
-- bundle can be hashed once the bundles it references have names. A spawn
-- cycle (a worker that transitively spawns itself) has no such order and
-- is reported as an error.


plan :: Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> Either [String] [Opt.Global]
plan (Opt.GlobalGraph nodes _) mains =
  let
    mainRoots =
      Map.foldrWithKey (\home _ gs -> Opt.Global home "main" : gs) [] mains

    mainRefs =
      Set.toList (refsFrom nodes (mainDecoderRefs mains) mainRoots)
  in
  case foldM_dfs nodes ([], Set.empty, Set.empty) mainRefs of
    Left cycleNames -> Left cycleNames
    Right (revOrder, _, _) -> Right (reverse revOrder)


-- depth-first: emit a worker after the workers it spawns
foldM_dfs
  :: Map.Map Opt.Global Opt.Node
  -> ([Opt.Global], Set.Set Opt.Global, Set.Set Opt.Global)
  -> [Opt.Global]
  -> Either [String] ([Opt.Global], Set.Set Opt.Global, Set.Set Opt.Global)
foldM_dfs nodes state roots =
  case roots of
    [] ->
      Right state

    root : rest ->
      do  state1 <- visit nodes state root
          foldM_dfs nodes state1 rest


visit
  :: Map.Map Opt.Global Opt.Node
  -> ([Opt.Global], Set.Set Opt.Global, Set.Set Opt.Global)
  -> Opt.Global
  -> Either [String] ([Opt.Global], Set.Set Opt.Global, Set.Set Opt.Global)
visit nodes state@(revOrder, done, stack) global =
  if Set.member global done then
    Right state
  else if Set.member global stack then
    Left (map globalToChars (Set.toList stack))
  else
    let
      refs = Set.toList (refsFrom nodes Set.empty [global])
    in
    do  (revOrder1, done1, _) <-
          foldM_dfs nodes (revOrder, done, Set.insert global stack) refs
        Right (global : revOrder1, Set.insert global done1, stack)



-- NAME TOKENS
--
-- A WorkerRef compiles to a placeholder token in the JavaScript output.
-- Once every worker bundle has been rendered and hashed, the tokens are
-- replaced with the final file names. The NUL bytes guarantee the token
-- cannot appear in user-written strings.


token :: Opt.Global -> BS.ByteString
token global =
  BS_UTF8.fromString ("\0elm-worker\0" ++ globalToChars global ++ "\0")


tokenBuilder :: Opt.Global -> B.Builder
tokenBuilder global =
  B.byteString (token global)


globalToChars :: Opt.Global -> String
globalToChars (Opt.Global (ModuleName.Canonical pkg modul) name) =
  Pkg.toChars pkg ++ "/" ++ Name.toChars modul ++ "#" ++ Name.toChars name



-- FIND WORKER REFS
--
-- All WorkerRef globals in the code reachable from the given roots.


mainDecoderRefs :: Map.Map ModuleName.Canonical Opt.Main -> Set.Set Opt.Global
mainDecoderRefs mains =
  Map.foldr addMain Set.empty mains
  where
    addMain main refs =
      case main of
        Opt.Static -> refs
        Opt.Dynamic _ decoder -> addExpr decoder refs


refsFrom :: Map.Map Opt.Global Opt.Node -> Set.Set Opt.Global -> [Opt.Global] -> Set.Set Opt.Global
refsFrom nodes initialRefs roots =
  let
    live = List.foldl' (addGlobal nodes) Set.empty roots
  in
  Set.foldl'
    (\refs global -> maybe refs (\node -> nodeRefs node refs) (Map.lookup global nodes))
    initialRefs
    live


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
    Opt.Define _ deps           -> deps
    Opt.DefineTailFunc _ _ deps -> deps
    Opt.Ctor _ _                -> Set.empty
    Opt.Tag _                   -> Set.empty
    Opt.Enum _                  -> Set.empty
    Opt.Box                     -> Set.empty
    Opt.Link global             -> Set.singleton global
    Opt.Cycle _ _ _ deps        -> deps
    Opt.Manager _               -> Set.empty
    Opt.Kernel _ deps           -> deps
    Opt.PortIncoming _ deps     -> deps
    Opt.PortOutgoing _ deps     -> deps
    Opt.PortTask _ _ deps       -> deps


nodeRefs :: Opt.Node -> Set.Set Opt.Global -> Set.Set Opt.Global
nodeRefs node refs =
  case node of
    Opt.Define expr _           -> addExpr expr refs
    Opt.DefineTailFunc _ expr _ -> addExpr expr refs
    Opt.Cycle _ pairs defs _    -> foldr (addExpr . snd) (foldr addDef refs defs) pairs
    Opt.PortIncoming expr _     -> addExpr expr refs
    Opt.PortOutgoing expr _     -> addExpr expr refs
    Opt.PortTask e1 e2 _        -> addExpr e1 (addExpr e2 refs)
    _                           -> refs


addDef :: Opt.Def -> Set.Set Opt.Global -> Set.Set Opt.Global
addDef def refs =
  case def of
    Opt.Def _ expr -> addExpr expr refs
    Opt.TailDef _ _ expr -> addExpr expr refs


addExpr :: Opt.Expr -> Set.Set Opt.Global -> Set.Set Opt.Global
addExpr expression refs =
  case expression of
    Opt.WorkerRef global -> Set.insert global refs

    Opt.Bool _ -> refs
    Opt.Chr _ -> refs
    Opt.Str _ -> refs
    Opt.Int _ -> refs
    Opt.Float _ -> refs
    Opt.VarLocal _ -> refs
    Opt.VarGlobal _ -> refs
    Opt.VarEnum _ _ -> refs
    Opt.VarBox _ -> refs
    Opt.VarCycle _ _ -> refs
    Opt.VarDebug _ _ _ _ -> refs
    Opt.VarKernel _ _ -> refs
    Opt.List exprs -> foldr addExpr refs exprs
    Opt.Function _ body -> addExpr body refs
    Opt.Call func args -> addExpr func (foldr addExpr refs args)
    Opt.TailCall _ args -> foldr (addExpr . snd) refs args
    Opt.If branches final ->
      foldr (\(a, b) rs -> addExpr a (addExpr b rs)) (addExpr final refs) branches
    Opt.Let def body -> addDef def (addExpr body refs)
    Opt.Destruct _ body -> addExpr body refs
    Opt.Case _ _ decider jumps ->
      addDecider decider (foldr (addExpr . snd) refs jumps)
    Opt.Accessor _ -> refs
    Opt.Access record _ -> addExpr record refs
    Opt.Update record fields -> addExpr record (Map.foldr addExpr refs fields)
    Opt.Record fields -> Map.foldr addExpr refs fields
    Opt.Unit -> refs
    Opt.Tuple a b maybeC -> addExpr a (addExpr b (foldr addExpr refs maybeC))
    Opt.Shader _ _ _ -> refs
    Opt.Css _ _ -> refs


addDecider :: Opt.Decider Opt.Choice -> Set.Set Opt.Global -> Set.Set Opt.Global
addDecider decider refs =
  case decider of
    Opt.Leaf choice ->
      case choice of
        Opt.Inline expr -> addExpr expr refs
        Opt.Jump _ -> refs

    Opt.Chain _ success failure ->
      addDecider success (addDecider failure refs)

    Opt.FanOut _ tests fallback ->
      foldr (addDecider . snd) (addDecider fallback refs) tests
