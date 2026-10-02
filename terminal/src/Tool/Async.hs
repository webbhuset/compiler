{-# LANGUAGE OverloadedStrings #-}
module Tool.Async
  ( run
  , bundle
  , moduleToChars
  , kB
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Text.Printf (printf)

import qualified String as S

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.String as ES
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Optimize.DecisionTree as DT
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project



-- RUN
--
--   elm tool async src/Main.elm
--
-- How many bytes each module adds to the bundle of an --optimize build, and
-- whether the program always needs it or only reaches it under some branch,
-- which is what makes a module worth an `import async`.
--
-- Own is the code of the module itself. Retained is what would leave the
-- bundle if the module did: its own code plus everything only it uses.
--
-- A module is eager when main reaches it without passing through a branch
-- of a `case` or an `if`, and conditional otherwise. Calling a function
-- counts as running it, so this is about branches, not about when a function
-- happens to be called: a module used only by `update`, outside any branch,
-- is still eager.


run :: FilePath -> IO (Either Problem Output)
run path =
  Project.withMain path $ \graph mains sizeOf ->
    case Map.toList mains of
      [(home, main)] ->
        return (Right (report (analyze graph sizeOf home main)))

      _ ->
        return (Left (BadName "a file with a `main` in it" path))



-- ANALYSIS


data Analysis =
  Analysis
    { _total :: Int
    , _home :: ModuleName.Canonical
    , _modules :: [ModuleInfo]
    , _chunks :: [(ModuleName.Canonical, Int)]
      -- modules that are already async, with the size of their chunks
    }


data ModuleInfo =
  ModuleInfo
    { _name :: ModuleName.Canonical
    , _own :: Int
    , _retained :: Int
    , _eager :: Bool
    , _entries :: [(Opt.Global, [String])]
      -- where a conditional module is first reached: the definition the
      -- branch is in, and what the branch tests
    , _importers :: [ModuleName.Canonical]
    , _suggested :: Bool
    }


-- Every global an --optimize build of the program would contain.
bundle :: Opt.GlobalGraph -> ModuleName.Canonical -> Opt.Main -> Set.Set Opt.Global
bundle (Opt.GlobalGraph nodes _) home main =
  let
    edges g = maybe [] (if isDebugger g then const [] else edgesOf g) (Map.lookup g nodes)
  in
  closure (map fst . edges) (Opt.Global home N.main : map fst (mainRefs main))


analyze :: Opt.GlobalGraph -> (Opt.Global -> Int) -> ModuleName.Canonical -> Opt.Main -> Analysis
analyze (Opt.GlobalGraph nodes _) sizeOf home main =
  let
    mainGlobal = Opt.Global home N.main
    roots = mainGlobal : map fst (mainRefs main)

    -- an --optimize build leaves the debugger out, and with it what only
    -- the debugger uses
    edges = Map.mapWithKey (\g n -> if isDebugger g then [] else edgesOf g n) nodes
    edgesFrom g = Map.findWithDefault [] g edges

    inBundle = closure (map fst . edgesFrom) roots
    direct = closure (\g -> [ d | (d, Nothing) <- edgesFrom g ]) roots

    moduleOf (Opt.Global m _) = m
    sizes = Map.fromSet sizeOf inBundle
    own = Map.fromListWith (+) [ (moduleOf g, n) | (g, n) <- Map.toList sizes ]
    total = sum (Map.elems own)

    moduleEdges =
      Map.fromListWith Set.union
        [ (moduleOf g, Set.singleton (moduleOf d))
        | g <- Set.toList inBundle, (d, _) <- edgesFrom g, moduleOf d /= moduleOf g
        ]

    importersOf =
      Map.fromListWith (++) [ (to, [from]) | (from, tos) <- Map.toList moduleEdges, to <- Set.toList tos ]

    retainedBy m =
      if m == home then total else
        let
          reached = closureSet (\x -> if x == m then [] else Set.toList (Map.findWithDefault Set.empty x moduleEdges)) [home]
          kept = sum [ Map.findWithDefault 0 x own | x <- Set.toList reached, x /= m ]
        in
        total - kept

    eagerModules = Set.map moduleOf direct

    -- branch edges leaving the always-run part of the program
    branchEdges =
      [ (from, to, tests)
      | from <- Set.toList direct, (to, Just tests) <- edgesFrom from, Set.notMember to direct
      ]

    reachedModules to =
      Set.map moduleOf (closure (map fst . edgesFrom) [to])

    entryTable =
      Map.fromListWith (++)
        [ (m, [(from, tests)])
        | (from, to, tests) <- branchEdges
        , m <- Set.toList (reachedModules to)
        , Set.notMember m eagerModules
        ]

    infos =
      [ ModuleInfo
          { _name = m
          , _own = size
          , _retained = retainedBy m
          , _eager = Set.member m eagerModules
          , _entries = List.nubBy (\a b -> fst a == fst b) (Map.findWithDefault [] m entryTable)
          , _importers = List.sort (Map.findWithDefault [] m importersOf)
          , _suggested = False
          }
      | (m, size) <- Map.toList own
      ]

    worth i =
      not (_eager i) && _retained i >= max 2048 (total `div` 50)
        && any (`Set.member` eagerModules) (_importers i)

    asyncTargets =
      Map.fromListWith Set.union
        [ (moduleOf a, Set.singleton a) | g <- Set.toList inBundle, a <- asyncRefsOf (Map.lookup g nodes) ]

    chunkSize targets =
      sum [ sizeOf g | g <- Set.toList (closure (map fst . edgesFrom) (Set.toList targets)), Set.notMember g inBundle ]
  in
  Analysis
    { _total = total
    , _home = home
    , _modules = List.sortOn (\i -> (negate (_retained i), negate (_own i))) [ i { _suggested = worth i } | i <- infos ]
    , _chunks = [ (m, chunkSize ts) | (m, ts) <- Map.toList asyncTargets ]
    }


isDebugger :: Opt.Global -> Bool
isDebugger (Opt.Global (ModuleName.Canonical pkg m) _) =
  pkg == Pkg.kernel && Module.toChars m == "Debugger"


closure :: (Opt.Global -> [Opt.Global]) -> [Opt.Global] -> Set.Set Opt.Global
closure =
  closureSet


closureSet :: (Ord a) => (a -> [a]) -> [a] -> Set.Set a
closureSet next =
  go Set.empty
  where
    go seen todo =
      case todo of
        [] -> seen
        x : rest
          | Set.member x seen -> go seen rest
          | otherwise         -> go (Set.insert x seen) (next x ++ rest)



-- EDGES
--
-- A dependency is direct when some reference to it is outside every branch,
-- and otherwise carries what the innermost branch around it tests.


type Label = Maybe [String]


edgesOf :: Opt.Global -> Opt.Node -> [(Opt.Global, Label)]
edgesOf global node =
  let
    found = Map.fromListWith better (nodeRefs node)
    better a b = if Maybe.isNothing a || Maybe.isNothing b then Nothing else a
  in
  [ (d, Map.findWithDefault Nothing d found) | d <- Set.toList (nodeDeps global node) ]


nodeDeps :: Opt.Global -> Opt.Node -> Set.Set Opt.Global
nodeDeps (Opt.Global home _) node =
  case node of
    Opt.Define _ deps           -> deps
    Opt.DefineTailFunc _ _ deps -> deps
    Opt.Ctor _ _                -> Set.empty
    Opt.Tag _                   -> Set.empty
    Opt.Enum _                  -> Set.empty
    Opt.Box                     -> Set.singleton (Opt.Global ModuleName.basics N.identity)
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
  Set.fromList $ map (Opt.Global home . N.fromString . S.fromChars) $
    [ "init", "onEffects", "onSelfMsg" ] ++
      case effectsType of
        Opt.Cmd -> [ "cmdMap" ]
        Opt.Sub -> [ "subMap" ]
        Opt.Fx  -> [ "cmdMap", "subMap" ]


mainRefs :: Opt.Main -> [(Opt.Global, Label)]
mainRefs main =
  case main of
    Opt.Dynamic _ decoder -> exprRefs Nothing decoder
    _                     -> []


nodeRefs :: Opt.Node -> [(Opt.Global, Label)]
nodeRefs node =
  case node of
    Opt.Define expr _           -> exprRefs Nothing expr
    Opt.DefineTailFunc _ expr _ -> exprRefs Nothing expr
    Opt.Cycle _ pairs defs _    -> concatMap (exprRefs Nothing . snd) pairs ++ concatMap (defRefs Nothing) defs
    Opt.PortIncoming expr _     -> exprRefs Nothing expr
    Opt.PortOutgoing expr _     -> exprRefs Nothing expr
    Opt.PortTask e1 e2 _        -> exprRefs Nothing e1 ++ exprRefs Nothing e2
    _                           -> []


defRefs :: Label -> Opt.Def -> [(Opt.Global, Label)]
defRefs label def =
  case def of
    Opt.Def _ expr       -> exprRefs label expr
    Opt.TailDef _ _ expr -> exprRefs label expr


exprRefs :: Label -> Opt.Expr -> [(Opt.Global, Label)]
exprRefs label expression =
  let
    go = exprRefs label
    under tests = Just (maybe tests (++ tests) label)
  in
  case expression of
    Opt.VarGlobal g             -> [(g, label)]
    Opt.VarEnum g _             -> [(g, label)]
    Opt.VarBox g                -> [(g, label)]
    Opt.VarCycle home name      -> [(Opt.Global home name, label)]
    Opt.List es                 -> concatMap go es
    Opt.Function _ body         -> go body
    Opt.Call f args             -> concatMap go (f : args)
    Opt.TailCall _ args         -> concatMap (go . snd) args
    Opt.TailBuild _ _ cell args -> go cell ++ concatMap (go . snd) args
    Opt.If branches final       ->
      case branches of
        [] -> go final
        (c, _) : _ ->
          go c
          ++ concat [ exprRefs (under ["if"]) b | (_, b) <- branches ]
          ++ concat [ exprRefs (under ["if"]) c' | (c', _) <- drop 1 branches ]
          ++ exprRefs (under ["else"]) final
    Opt.Let def body            -> defRefs label def ++ go body
    Opt.Destruct _ body         -> go body
    Opt.Case _ _ decider jumps  ->
      let
        leaves = deciderLeaves [] decider
        jumpTests = Map.fromListWith (++) [ (i, tests) | (tests, Opt.Jump i) <- leaves ]
      in
      concat [ exprRefs (under tests) e | (tests, Opt.Inline e) <- leaves ]
      ++ concat [ exprRefs (under (List.nub (Map.findWithDefault [] i jumpTests))) e | (i, e) <- jumps ]
    Opt.Access record _         -> go record
    Opt.Update record fields    -> go record ++ concatMap go (Map.elems fields)
    Opt.Record fields           -> concatMap go (Map.elems fields)
    Opt.Pair a b                -> go a ++ go b
    Opt.Triple a b c            -> go a ++ go b ++ go c
    _                           -> []


-- Each leaf of a decision tree, with the tests that lead to it.
deciderLeaves :: [String] -> Opt.Decider Opt.Choice -> [([String], Opt.Choice)]
deciderLeaves tests decider =
  case decider of
    Opt.Leaf choice ->
      [(tests, choice)]

    Opt.Chain chain success failure ->
      let tested = Maybe.mapMaybe (testToChars . snd) chain in
      deciderLeaves (tests ++ tested) success
      ++ deciderLeaves (tests ++ [notTested tested]) failure

    Opt.FanOut _ options fallback ->
      concat [ deciderLeaves (tests ++ Maybe.maybeToList (testToChars t)) d | (t, d) <- options ]
      ++ deciderLeaves (tests ++ [notTested (Maybe.mapMaybe (testToChars . fst) options)]) fallback


-- A fallback names what it is not, which for a `case` on a custom type is
-- usually the one constructor left.
notTested :: [String] -> String
notTested tested =
  case tested of
    [] -> "_"
    _  -> "not " ++ List.intercalate "/" tested


testToChars :: DT.Test -> Maybe String
testToChars test =
  case test of
    DT.IsCtor (ModuleName.Canonical _ m) name _ _ _ -> Just (Module.toChars m ++ "." ++ N.toChars name)
    DT.IsTag (ModuleName.Canonical _ m) name        -> Just (Module.toChars m ++ "." ++ N.toChars name)
    DT.IsCons                                       -> Just "::"
    DT.IsNil                                        -> Just "[]"
    DT.IsTuple                                      -> Nothing
    DT.IsInt n                                      -> Just (show n)
    DT.IsChr c                                      -> Just (show c)
    DT.IsStr s                                      -> Just (show (ES.toChars s))
    DT.IsBool b                                     -> Just (if b then "True" else "False")


asyncRefsOf :: Maybe Opt.Node -> [Opt.Global]
asyncRefsOf maybeNode =
  case maybeNode of
    Just (Opt.Define expr _)           -> asyncRefs expr
    Just (Opt.DefineTailFunc _ expr _) -> asyncRefs expr
    Just (Opt.Cycle _ pairs defs _)    -> concatMap (asyncRefs . snd) pairs ++ concatMap (\d -> case d of Opt.Def _ e -> asyncRefs e; Opt.TailDef _ _ e -> asyncRefs e) defs
    _                                  -> []


asyncRefs :: Opt.Expr -> [Opt.Global]
asyncRefs expression =
  let go = asyncRefs in
  case expression of
    Opt.AsyncRef g              -> [g]
    Opt.List es                 -> concatMap go es
    Opt.Function _ body         -> go body
    Opt.Call f args             -> concatMap go (f : args)
    Opt.TailCall _ args         -> concatMap (go . snd) args
    Opt.TailBuild _ _ cell args -> go cell ++ concatMap (go . snd) args
    Opt.If branches final       -> concat [ go c ++ go b | (c, b) <- branches ] ++ go final
    Opt.Let def body            -> (case def of Opt.Def _ e -> go e; Opt.TailDef _ _ e -> go e) ++ go body
    Opt.Destruct _ body         -> go body
    Opt.Case _ _ decider jumps  -> concat [ go e | (_, Opt.Inline e) <- deciderLeaves [] decider ] ++ concatMap (go . snd) jumps
    Opt.Access record _         -> go record
    Opt.Update record fields    -> go record ++ concatMap go (Map.elems fields)
    Opt.Record fields           -> concatMap go (Map.elems fields)
    Opt.Pair a b                -> go a ++ go b
    Opt.Triple a b c            -> go a ++ go b ++ go c
    _                           -> []



-- REPORT


report :: Analysis -> Output
report analysis@(Analysis total _ modules chunks) =
  let
    suggested = filter _suggested modules

    row i =
      printf "%9s %9s  %-11s %s%s\n"
        (kB (_own i)) (kB (_retained i)) (status analysis i) (moduleToChars (_name i)) (whereText i)

    whereText i =
      case _entries i of
        [] -> ""
        es -> "  " ++ List.intercalate "; " (map entryToChars (take 2 es)) ++ (if length es > 2 then "; ..." else "")

    suggestion i =
      "    import async " ++ moduleToChars (_name i) ++ "  -- " ++ kB (_retained i)
      ++ " kB, in " ++ List.intercalate ", " (map moduleToChars (_importers i)) ++ "\n"
  in
  Output
    ( "Bundle: " ++ kB total ++ " kB in " ++ show (length modules) ++ " modules (--optimize, before minification)\n\n"
      ++ printf "%9s %9s  %-11s %s\n" ("own kB" :: String) ("retained" :: String) ("use" :: String) ("module" :: String)
      ++ concatMap row modules
      ++ ( if null chunks then "" else
             "\nAlready async:\n" ++ concat [ "    " ++ moduleToChars m ++ "  -- chunk of " ++ kB n ++ " kB\n" | (m, n) <- chunks ]
         )
      ++ ( if null suggested then "\nNo module looks worth an `import async`.\n" else
             "\nWorth an `import async`, in every module listed after it:\n" ++ concatMap suggestion suggested
         )
    )
    ( E.object
        [ "total" ==> E.int total
        , "modules" ==> E.list (moduleToJson analysis) modules
        , "async" ==> E.list (\(m, n) -> E.object [ "module" ==> E.chars (moduleToChars m), "chunk" ==> E.int n ]) chunks
        ]
    )


status :: Analysis -> ModuleInfo -> String
status analysis i
  | _name i == _home analysis = "main"
  | _eager i                  = "eager"
  | otherwise                 = "conditional"


entryToChars :: (Opt.Global, [String]) -> String
entryToChars (Opt.Global home name, tests) =
  "in " ++ moduleToChars home ++ "." ++ N.toChars name ++ " under " ++ List.intercalate " > " tests


moduleToJson :: Analysis -> ModuleInfo -> E.Value
moduleToJson analysis i =
  E.object
    [ "module" ==> E.chars (moduleToChars (_name i))
    , "package" ==> E.chars (Pkg.toChars (ModuleName._package (_name i)))
    , "own" ==> E.int (_own i)
    , "retained" ==> E.int (_retained i)
    , "use" ==> E.chars (status analysis i)
    , "entries" ==> E.list (\(Opt.Global (ModuleName.Canonical _ m) name, tests) ->
          E.object [ "in" ==> E.chars (Module.toChars m ++ "." ++ N.toChars name), "under" ==> E.list E.chars tests ]) (_entries i)
    , "importers" ==> E.list (E.chars . moduleToChars) (_importers i)
    , "suggested" ==> E.bool (_suggested i)
    ]


moduleToChars :: ModuleName.Canonical -> String
moduleToChars (ModuleName.Canonical pkg m) =
  (if pkg == Pkg.kernel then "Elm.Kernel." else "") ++ Module.toChars m


kB :: Int -> String
kB n =
  printf "%.1f" (fromIntegral n / 1024 :: Double)
