{-# LANGUAGE OverloadedStrings #-}
module Generate.Chunks
  ( Plan(..)
  , Chunk(..)
  , Program(..)
  , plan
  , insideWorkers
  , getName
  , regName
  , scopeName
  , scopeArg
  , programName
  , appExport
  , token
  , tokenBuilder
  , homeToChars
  , runtime
  , scopeDef
  , registration
  , readyRegistration
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
import qualified Generate.JavaScript.Name as JsName



-- PLAN
--
-- Split the live graph into the main bundle and one chunk per module that
-- was reached only through an `import async`. See
-- docs/code-splitting-design.md for the rules; the short version is that a
-- chunk owns what only it needs, and everything else -- code a second
-- chunk also wants, effect managers, kernel code -- stays in main.
--
-- When several applications are compiled into one ES module, each one's
-- `main` roots a chunk of its own as well, so the main bundle holds only
-- what they share and an application's code is fetched when it is started.


data Plan =
  Plan
    { _chunks :: [Chunk]
      -- in dependency order: a chunk comes after every chunk it references,
      -- so it can be hashed once those have file names
    , _mainSeen :: Set.Set Opt.Global
      -- everything the main bundle emits, including code hoisted out of the
      -- chunks; seeds the main walk and blocks it in each chunk walk
    }


data Chunk =
  Chunk
    { _home :: ModuleName.Canonical
    , _roots :: Set.Set Opt.Global
      -- the values referenced across the boundary: the chunk's walk starts
      -- here and its exports are exactly these
    , _program :: Maybe Program
      -- set when this chunk holds an application the bundle exports; its
      -- `init` then fetches the chunk instead of finding `main` in scope
    }


-- An application that lives in a chunk, with the ports a host page can
-- reach on it: the incoming ones (`send`) and the outgoing ones
-- (`subscribe`/`unsubscribe`). They are known here, before the chunk
-- arrives, which is what lets `init` hand back a usable app object at once.
data Program =
  Program
    { _main :: Opt.Main
    , _incomingPorts :: [Name.Name]
    , _outgoingPorts :: [Name.Name]
    }


plan :: Bool -> Bool -> Opt.GlobalGraph -> Map.Map ModuleName.Canonical Opt.Main -> Either [String] (Maybe Plan)
plan isDebug splitApps (Opt.GlobalGraph allNodes _) mains =
  let
    nodes =
      if isDebug then allNodes else Map.filterWithKey (\g _ -> not (isDebugger g)) allNodes

    mainRoots =
      Map.foldrWithKey (\home _ gs -> Opt.Global home "main" : gs) [] mains

    -- Splitting applications means the main bundle starts empty and each
    -- `main` is a chunk root; what the applications share, effect managers
    -- and kernel code then land in main by the ordinary hoisting rule.
    (seed, appRoots) =
      if splitApps then
        ( Set.empty
        , Map.fromList [ (home, Set.singleton global) | global@(Opt.Global home _) <- mainRoots ]
        )
      else
        ( closure nodes Set.empty mainRoots
        , Map.empty
        )

    toProgram home =
      if splitApps then
        fmap (program nodes home) (Map.lookup home mains)
      else
        Nothing
  in
  case Map.toList (settle nodes seed appRoots) of
    [] ->
      Right Nothing

    chunkList ->
      let
        mainSeen = grow nodes seed (map snd chunkList)
        table = Map.fromList chunkList
      in
      case order nodes mainSeen chunkList of
        Left cycleNames ->
          Left cycleNames

        Right ordered ->
          Right $ Just $ Plan
            [ Chunk home (table Map.! home) (toProgram home) | home <- ordered ]
            mainSeen


program :: Map.Map Opt.Global Opt.Node -> ModuleName.Canonical -> Opt.Main -> Program
program nodes home main =
  let
    live = Set.toList (closure nodes Set.empty [Opt.Global home "main"])

    portsOf isKind =
      [ name
      | global@(Opt.Global _ name) <- live
      , maybe False isKind (Map.lookup global nodes)
      ]

    isIncoming node = case node of { Opt.PortIncoming _ _ -> True ; _ -> False }
    isOutgoing node = case node of { Opt.PortOutgoing _ _ -> True ; _ -> False }
  in
  Program main (portsOf isIncoming) (portsOf isOutgoing)



-- A worker bundle is a self-contained file with its own copy of everything
-- it needs, so it has no main bundle scope to fetch a chunk into. An async
-- import reached from a worker program is an error rather than a runtime
-- surprise; the answer is to import that module normally.
insideWorkers :: Bool -> Opt.GlobalGraph -> [Opt.Global] -> Maybe ModuleName.Canonical
insideWorkers isDebug (Opt.GlobalGraph allNodes _) workerRoots =
  let
    nodes =
      if isDebug then allNodes else Map.filterWithKey (\g _ -> not (isDebugger g)) allNodes
  in
  case Map.keys (asyncRefsIn nodes (closure nodes Set.empty workerRoots)) of
    home : _ -> Just home
    []       -> Nothing



-- WHICH MODULES BECOME CHUNKS
--
-- An async reference in the main bundle names a chunk; that chunk's own
-- code can name further chunks, so this runs to a fixed point. The value
-- is every referenced global per module, which is also that chunk's export
-- list. The caller may hand in chunks that exist for another reason -- an
-- application's `main` when applications are split -- and those are
-- walked for async references like any other.


settle
  :: Map.Map Opt.Global Opt.Node
  -> Set.Set Opt.Global
  -> Map.Map ModuleName.Canonical (Set.Set Opt.Global)
  -> Map.Map ModuleName.Canonical (Set.Set Opt.Global)
settle nodes seen given =
  go (Map.unionWith Set.union given (asyncRefsIn nodes seen)) Map.empty
  where
    go found acc =
      let
        merged = Map.unionWith Set.union acc found
      in
      if merged == acc then
        acc
      else
        go (Map.foldr (\roots fs ->
              Map.unionWith Set.union fs
                (asyncRefsIn nodes (closure nodes Set.empty (Set.toList roots))))
              Map.empty merged)
           merged


asyncRefsIn
  :: Map.Map Opt.Global Opt.Node
  -> Set.Set Opt.Global
  -> Map.Map ModuleName.Canonical (Set.Set Opt.Global)
asyncRefsIn nodes live =
  Set.foldl'
    (\acc global ->
        case Map.lookup global nodes of
          Nothing -> acc
          Just node -> nodeAsyncRefs node acc)
    Map.empty
    live



-- WHAT THE MAIN BUNDLE KEEPS
--
-- A chunk gives up anything a second chunk also wants, so one realm never
-- holds two copies of a value, and anything the runtime registers once --
-- effect managers, kernel code -- since a late arrival could never be
-- wired up. Hoisting pulls in dependencies of its own, so this repeats
-- until nothing more moves.


grow
  :: Map.Map Opt.Global Opt.Node
  -> Set.Set Opt.Global
  -> [Set.Set Opt.Global]
  -> Set.Set Opt.Global
grow nodes seen chunkRoots =
  let
    lives = map (closure nodes Set.empty . Set.toList) chunkRoots

    counts =
      List.foldl'
        (\acc live -> Set.foldl' (\a g -> Map.insertWith (+) g (1 :: Int) a) acc live)
        Map.empty
        lives

    hoisted =
      Set.fromList
        [ global
        | (global, n) <- Map.toList counts
        , not (Set.member global seen)
        , n > 1 || staysInMain (Map.lookup global nodes)
        ]
  in
  if Set.null hoisted then
    seen
  else
    grow nodes (closure nodes seen (Set.toList hoisted)) chunkRoots


-- An effect manager is registered when the program starts and kernel code
-- is emitted as one prelude ahead of everything else; neither survives
-- arriving later, so both stay in the main bundle.
staysInMain :: Maybe Opt.Node -> Bool
staysInMain maybeNode =
  case maybeNode of
    Just (Opt.Manager _) -> True
    Just (Opt.Kernel _ _) -> True
    _ -> False



-- ORDER
--
-- A chunk that names another has to be hashed after it, exactly as a
-- worker bundle does, so the chunks are emitted depth-first and a cycle
-- between them is an error.


order
  :: Map.Map Opt.Global Opt.Node
  -> Set.Set Opt.Global
  -> [(ModuleName.Canonical, Set.Set Opt.Global)]
  -> Either [String] [ModuleName.Canonical]
order nodes mainSeen chunkList =
  let
    table = Map.fromList chunkList

    references home =
      case Map.lookup home table of
        Nothing -> []
        Just roots ->
          Map.keys (asyncRefsIn nodes (own nodes mainSeen roots))
  in
  case visitAll references ([], Set.empty, Set.empty) (map fst chunkList) of
    Left cycleNames ->
      Left cycleNames

    Right (revOrder, _, _) ->
      Right (reverse revOrder)


visitAll
  :: (ModuleName.Canonical -> [ModuleName.Canonical])
  -> ([ModuleName.Canonical], Set.Set ModuleName.Canonical, Set.Set ModuleName.Canonical)
  -> [ModuleName.Canonical]
  -> Either [String] ([ModuleName.Canonical], Set.Set ModuleName.Canonical, Set.Set ModuleName.Canonical)
visitAll references state homes =
  case homes of
    [] ->
      Right state

    home : rest ->
      do  state1 <- visit references state home
          visitAll references state1 rest


visit
  :: (ModuleName.Canonical -> [ModuleName.Canonical])
  -> ([ModuleName.Canonical], Set.Set ModuleName.Canonical, Set.Set ModuleName.Canonical)
  -> ModuleName.Canonical
  -> Either [String] ([ModuleName.Canonical], Set.Set ModuleName.Canonical, Set.Set ModuleName.Canonical)
visit references state@(revOrder, done, stack) home =
  if Set.member home done then
    Right state
  else if Set.member home stack then
    Left (map homeToChars (Set.toList stack))
  else
    do  (revOrder1, done1, _) <-
          visitAll references (revOrder, done, Set.insert home stack) (references home)
        Right (home : revOrder1, Set.insert home done1, stack)



-- WHAT ONE CHUNK OWNS


own :: Map.Map Opt.Global Opt.Node -> Set.Set Opt.Global -> Set.Set Opt.Global -> Set.Set Opt.Global
own nodes mainSeen roots =
  Set.difference (closure nodes Set.empty (Set.toList roots)) mainSeen



-- Without --debug the code generator drops the debugger's kernel node, and
-- with it everything only the debugger needs. Dropping it here too keeps
-- the partition matching what actually comes out.
isDebugger :: Opt.Global -> Bool
isDebugger (Opt.Global (ModuleName.Canonical _ home) _) =
  home == Name.debugger



-- REACHABILITY
--
-- The same edges Generate.JavaScript walks when it emits, so a partition
-- decided here matches what actually comes out: dependency sets, plus the
-- two the code generator adds itself -- a Box pulls in Basics.identity and
-- a Manager pulls in the functions it is built from.


closure :: Map.Map Opt.Global Opt.Node -> Set.Set Opt.Global -> [Opt.Global] -> Set.Set Opt.Global
closure nodes seen roots =
  List.foldl' (addGlobal nodes) seen roots


addGlobal :: Map.Map Opt.Global Opt.Node -> Set.Set Opt.Global -> Opt.Global -> Set.Set Opt.Global
addGlobal nodes seen global =
  if Set.member global seen then
    seen
  else
    Set.foldl' (addGlobal nodes) (Set.insert global seen)
      (maybe Set.empty (nodeDeps global) (Map.lookup global nodes))


nodeDeps :: Opt.Global -> Opt.Node -> Set.Set Opt.Global
nodeDeps (Opt.Global home _) node =
  case node of
    Opt.Define _ deps           -> deps
    Opt.DefineTailFunc _ _ deps -> deps
    Opt.Ctor _ _                -> Set.empty
    Opt.Tag _                   -> Set.empty
    Opt.Enum _                  -> Set.empty
    Opt.Box                     -> Set.singleton (Opt.Global ModuleName.basics Name.identity)
    Opt.Link global             -> Set.singleton global
    Opt.Cycle _ _ _ deps        -> deps
    Opt.Manager effectsType     -> managerDeps home effectsType
    Opt.Kernel _ deps           -> deps
    Opt.PortIncoming _ deps     -> deps
    Opt.PortOutgoing _ deps     -> deps
    Opt.PortTask _ _ deps       -> deps


-- Mirrors Generate.JavaScript.generateManagerHelp.
managerDeps :: ModuleName.Canonical -> Opt.EffectsType -> Set.Set Opt.Global
managerDeps home effectsType =
  Set.fromList $ map (Opt.Global home) $
    [ "init", "onEffects", "onSelfMsg" ] ++
      case effectsType of
        Opt.Cmd -> [ "cmdMap" ]
        Opt.Sub -> [ "subMap" ]
        Opt.Fx  -> [ "cmdMap", "subMap" ]



-- FIND ASYNC REFS
--
-- Flags decoders, ports and program registrations are optimized as if
-- nothing were async (Optimize.Module.noAsyncs), so only ordinary
-- definitions can hold one.


nodeAsyncRefs :: Opt.Node -> Map.Map ModuleName.Canonical (Set.Set Opt.Global) -> Map.Map ModuleName.Canonical (Set.Set Opt.Global)
nodeAsyncRefs node refs =
  case node of
    Opt.Define expr _           -> addExpr expr refs
    Opt.DefineTailFunc _ expr _ -> addExpr expr refs
    Opt.Cycle _ pairs defs _    -> foldr (addExpr . snd) (foldr addDef refs defs) pairs
    Opt.PortIncoming expr _     -> addExpr expr refs
    Opt.PortOutgoing expr _     -> addExpr expr refs
    Opt.PortTask e1 e2 _        -> addExpr e1 (addExpr e2 refs)
    _                           -> refs


type Refs =
  Map.Map ModuleName.Canonical (Set.Set Opt.Global)


addDef :: Opt.Def -> Refs -> Refs
addDef def refs =
  case def of
    Opt.Def _ expr -> addExpr expr refs
    Opt.TailDef _ _ expr -> addExpr expr refs


addExpr :: Opt.Expr -> Refs -> Refs
addExpr expression refs =
  case expression of
    Opt.AsyncRef global@(Opt.Global home _) ->
      Map.insertWith Set.union home (Set.singleton global) refs

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
    Opt.WorkerRef _ -> refs
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


addDecider :: Opt.Decider Opt.Choice -> Refs -> Refs
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



-- RUNTIME NAMES
--
-- Defined by the chunk prelude in Generate.JavaScript, which is emitted
-- only when a program actually has chunks.


getName :: JsName.Name
getName =
  JsName.fromKernel "Chunk" "get"


regName :: JsName.Name
regName =
  JsName.fromKernel "Chunk" "reg"


scopeName :: JsName.Name
scopeName =
  JsName.fromKernel "Chunk" "scope"


-- The parameter a chunk module's default export takes: the main bundle's
-- scope, which the chunk rebinds as locals before its own definitions.
scopeArg :: JsName.Name
scopeArg =
  JsName.fromKernel "Chunk" "s"


-- Builds the exported `init` of an application that lives in a chunk.
programName :: JsName.Name
programName =
  JsName.fromKernel "Chunk" "program"


-- The key under which an application chunk exports its started program:
-- `main` applied to its flags decoder and debug metadata, ready for the
-- host page's `args`. Built inside the chunk, so the decoder's own
-- dependencies are in scope wherever the planner put them.
appExport :: B.Builder
appExport =
  "_app"



-- RUNTIME
--
-- Emitted into the main bundle, and only when a program actually has
-- chunks, so everything else comes out byte for byte as before.
--
-- A chunk is fetched the first time one of its values is read. Until it is
-- there, reading throws a marker carrying the promise. Every place the Elm
-- runtime calls into user code -- init, update, view, subscriptions --
-- catches that marker, waits, and calls again; re-running is safe because
-- the code it re-runs is pure, and nothing it did the first time was
-- committed. The marker is an ordinary Error carrying a tag field, rather
-- than a class, so the patched elm/core and elm/browser can recognise it
-- without needing this prelude to exist -- and so that a reference from
-- somewhere the runtime cannot retry says so instead of surfacing as an
-- uncaught object. The tag is spelled without leading underscores on
-- purpose: `__name` in a kernel file is a token the kernel preprocessor
-- rewrites, and the two sides have to agree on one literal name.
--
-- An application that lives in a chunk is started through _Chunk_program.
-- Its `init` cannot wait: host pages call `app.ports.x.send` on the very
-- next line. So it returns an app object at once whose ports are known
-- from the graph, records what the page does with them, and replays it on
-- the real app once the chunk has landed and `main` has been started with
-- the same `args`. Nothing is lost in between: an outgoing port's first
-- messages are delivered after a `sleep 0` anyway, so a subscriber
-- registered right after the real start still sees them.


runtime :: B.Builder
runtime =
  "function _Chunk_reg(url) { return { u: url, e: null, p: null }; }\n\
  \function _Chunk_ready(exports) { return { u: null, e: exports, p: null }; }\n\
  \function _Chunk_load(c) {\n\
  \\tif (!c.p) {\n\
  \\t\tc.p = import(new URL(c.u, __elmWorkerBaseUrl)).then(function(m) {\n\
  \\t\t\tc.e = m.default(_Chunk_scope());\n\
  \\t\t});\n\
  \\t}\n\
  \\treturn c.p;\n\
  \}\n\
  \function _Chunk_get(c) {\n\
  \\tif (c.e) { return c.e; }\n\
  \\tvar e = new Error(\"An async-imported module is not here yet. Elm waits for one in init, update, view and subscriptions; this reference was somewhere else, such as inside a Task callback. See docs/code-splitting.md.\");\n\
  \\te.elmChunk = _Chunk_load(c);\n\
  \\tthrow e;\n\
  \}\n\
  \function _Chunk_program(c, incoming, outgoing) {\n\
  \\treturn function(args) {\n\
  \\t\tif (c.e) { return c.e." <> appExport <> "(args); }\n\
  \\t\tvar app = null;\n\
  \\t\tvar pending = [];\n\
  \\t\tfunction later(f) { app ? f() : pending.push(f); }\n\
  \\t\tvar ports = {};\n\
  \\t\tincoming.forEach(function(name) {\n\
  \\t\t\tports[name] = {\n\
  \\t\t\t\tsend: function(value) { later(function() { app.ports[name].send(value); }); }\n\
  \\t\t\t};\n\
  \\t\t});\n\
  \\t\toutgoing.forEach(function(name) {\n\
  \\t\t\tports[name] = {\n\
  \\t\t\t\tsubscribe: function(callback) { later(function() { app.ports[name].subscribe(callback); }); },\n\
  \\t\t\t\tunsubscribe: function(callback) { later(function() { app.ports[name].unsubscribe(callback); }); }\n\
  \\t\t\t};\n\
  \\t\t});\n\
  \\t\t_Chunk_load(c).then(function() {\n\
  \\t\t\tapp = c.e." <> appExport <> "(args);\n\
  \\t\t\tvar work = pending;\n\
  \\t\t\tpending = [];\n\
  \\t\t\tfor (var i = 0; i < work.length; i++) { work[i](); }\n\
  \\t\t});\n\
  \\t\treturn incoming.length || outgoing.length ? { ports: ports } : {};\n\
  \\t};\n\
  \}\n"


-- The main bundle's scope, handed to every chunk so it can rebind the
-- names it needs as locals. Built once, on the first chunk that loads.
scopeDef :: Set.Set BS.ByteString -> B.Builder
scopeDef names =
  "var _Chunk_scopeCache;\n\
  \function _Chunk_scope() {\n\
  \\treturn _Chunk_scopeCache || (_Chunk_scopeCache = {"
  <> mconcat (List.intersperse "," (map toField (Set.toList names)))
  <> "});\n}\n"
  where
    toField name =
      let b = B.byteString name in b <> ":" <> b


-- A module that is also reachable without crossing an async import is in
-- the main bundle already, so there is nothing left to put in a file. The
-- import degrades to a chunk that is born loaded: same reference sites, no
-- request, no file.
readyRegistration :: ModuleName.Canonical -> [B.Builder] -> B.Builder
readyRegistration home exports =
  "var " <> JsName.toBuilder (JsName.fromChunk home) <> " = _Chunk_ready({"
  <> mconcat (List.intersperse "," (map (\e -> e <> ":" <> e) exports))
  <> "});\n"


-- `var _Chunk$author$project$Big = _Chunk_reg("<token>");`
registration :: ModuleName.Canonical -> B.Builder
registration home =
  "var " <> JsName.toBuilder (JsName.fromChunk home) <> " = _Chunk_reg(\""
  <> tokenBuilder home <> "\");\n"



-- NAME TOKENS
--
-- A chunk's file name is not known until it has been rendered and hashed,
-- so the main bundle holds a NUL-delimited placeholder until then, exactly
-- as it does for worker bundles. The NUL bytes guarantee the token cannot
-- appear in a user-written string.


token :: ModuleName.Canonical -> BS.ByteString
token home =
  BS_UTF8.fromString ("\0elm-chunk\0" ++ homeToChars home ++ "\0")


tokenBuilder :: ModuleName.Canonical -> B.Builder
tokenBuilder home =
  B.byteString (token home)


homeToChars :: ModuleName.Canonical -> String
homeToChars (ModuleName.Canonical pkg modul) =
  Pkg.toChars pkg ++ "/" ++ Name.toChars modul
