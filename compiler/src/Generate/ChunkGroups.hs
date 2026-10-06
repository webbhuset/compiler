module Generate.ChunkGroups
  ( Config(..)
  , defaultConfig
  , Grouping(..)
  , Bundle(..)
  , Entry(..)
  , group
  , loads
  , fetches
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName
import qualified Generate.Chunks as Chunks



-- GROUPING CHUNKS INTO FILES
--
-- Generate.Chunks.plan makes one file per async import and hoists anything
-- two of them share into the main bundle. That keeps the planner simple
-- but grows main with code it never runs: if two pages share a large
-- library, the library is downloaded before either page is opened.
--
-- This planner separates "what an async import needs" from "which file it
-- is in". Every definition outside the main bundle is labelled with the set
-- of async imports that need it, and each distinct label is a bundle.
-- Loading an import means loading every bundle whose label contains it,
-- all of which are known when compiling, so they are fetched side by side
-- rather than one after another.
--
-- Labelling alone gives many tiny files, so bundles under a minimum size
-- are then merged, smallest first, into whichever neighbour wastes the
-- fewest bytes: the bytes some import would download without needing
-- them. The main bundle is a neighbour too: one every import has loaded,
-- so code there is wasted on each import that does not need it, and costs
-- a premium on top, since it delays the first screen. Code nearly every
-- import shares goes there; a page that only one import needs does not.
--
-- Nothing here generates code yet. It is the analysis `elm tool chunks`
-- reports, to check the grouping on real programs before the generator
-- and the loader learn to emit it.


data Config =
  Config
    { _minSize :: Int
      -- a bundle smaller than this, in bytes of generated code, is merged
    , _mainWeight :: Int
      -- what a byte in the main bundle costs on top of being downloaded by
      -- the imports that do not need it, in the same unit: one import
      -- downloading one byte it does not need
    }


-- Bytes of --optimize output before minification. 20 kB comes out at
-- about 5 kB once minified and compressed.
defaultConfig :: Config
defaultConfig =
  Config 20000 2


data Grouping =
  Grouping
    { _main :: Set.Set Opt.Global
    , _bundles :: [Bundle]
      -- in load order: a bundle comes after every bundle it uses
    , _entries :: Map.Map ModuleName.Canonical Entry
    }


data Bundle =
  Bundle
    { _label :: Set.Set ModuleName.Canonical
      -- the async imports that load this bundle
    , _globals :: Set.Set Opt.Global
    , _size :: Int
    }


data Entry =
  Entry
    { _needs :: Set.Set Opt.Global
      -- what the import needs that the main bundle does not have
    , _fromMain :: Bool
      -- whether code in the main bundle refers to it
    , _fromChunks :: Set.Set ModuleName.Canonical
      -- the async imports whose code refers to it
    , _parent :: Maybe ModuleName.Canonical
      -- set when only one other import's own code refers to it: that
      -- import is always in by the time this one is asked for, which is
      -- the waterfall to look out for
    }


-- The bundles one async import loads.
loads :: Grouping -> ModuleName.Canonical -> [Bundle]
loads grouping home =
  filter (Set.member home . _label) (_bundles grouping)


-- The bundles one async import downloads when it is asked for: those it
-- loads that no import certainly in before it has loaded already.
fetches :: Grouping -> ModuleName.Canonical -> [Bundle]
fetches grouping home =
  let
    before = ancestorsOf (Map.map _parent (_entries grouping)) home
  in
  filter (Set.null . Set.intersection before . _label) (loads grouping home)



-- GROUP


group
  :: Config -> (Opt.Global -> Int) -> Bool -> Bool -> Opt.GlobalGraph
  -> Map.Map ModuleName.Canonical Opt.Main -> Grouping
group config sizeOf isDebug splitApps graph mains =
  let
    Chunks.Discovery nodes seed found = Chunks.discover isDebug splitApps graph mains

    lives =
      Map.map (Chunks.closure nodes Set.empty . Set.toList) found

    -- effect managers and kernel code are registered at startup, so they
    -- stay in main whoever needs them, and so does what they use
    mainSet =
      Chunks.closure nodes seed
        [ global
        | global <- Set.toList (Set.unions (Map.elems lives))
        , Chunks.staysInMain (Map.lookup global nodes)
        ]

    needs =
      Map.map (\live -> Set.difference live mainSet) lives

    labels =
      Map.unionsWith Set.union
        [ Map.fromSet (const (Set.singleton home)) need
        | (home, need) <- Map.toList needs
        ]

    entries =
      toEntries nodes mainSet labels needs

    initial =
      Map.fromList (zip [0..] (Map.toList (invert labels)))

    groupOf =
      Map.fromList
        [ (global, i) | (i, (_, globals)) <- Map.toList initial, global <- Set.toList globals ]

    toGroup i (label, globals) =
      Group label globals (sum (map sizeOf (Set.toList globals)))
        (Set.delete i (Set.fromList
          [ j
          | global <- Set.toList globals
          , dep <- Set.toList (Chunks.dependencies nodes global)
          , Just j <- [ Map.lookup dep groupOf ]
          ]))

    ancestors =
      ancestorsOf (Map.map _parent entries)

    (mainExtra, groups) =
      mergeAll config (Map.size found) ancestors Set.empty (Map.mapWithKey toGroup initial)
  in
  Grouping
    { _main = Set.union mainSet mainExtra
    , _bundles = map toBundle (topological groups)
    , _entries = entries
    }


invert :: Map.Map Opt.Global (Set.Set ModuleName.Canonical) -> Map.Map (Set.Set ModuleName.Canonical) (Set.Set Opt.Global)
invert labels =
  Map.fromListWith Set.union [ (label, Set.singleton global) | (global, label) <- Map.toList labels ]


toEntries
  :: Map.Map Opt.Global Opt.Node
  -> Set.Set Opt.Global
  -> Map.Map Opt.Global (Set.Set ModuleName.Canonical)
  -> Map.Map ModuleName.Canonical (Set.Set Opt.Global)
  -> Map.Map ModuleName.Canonical Entry
toEntries nodes mainSet labels needs =
  let
    -- where each async import is referred from: Nothing for the main
    -- bundle, or the label of the code that holds the reference
    origins =
      Map.fromListWith Set.union
        [ (target, Set.singleton origin)
        | (global, origin) <-
            map (\g -> (g, Nothing)) (Set.toList mainSet)
            ++ map (fmap Just) (Map.toList labels)
        , target <- Set.toList (Chunks.asyncRefs nodes global)
        ]

    toEntry home need =
      let
        from = Map.findWithDefault Set.empty home origins
        chunkLabels = Maybe.catMaybes (Set.toList from)
      in
      Entry
        { _needs = need
        , _fromMain = Set.member Nothing from
        , _fromChunks = Set.delete home (Set.unions chunkLabels)
        , _parent =
            case Set.toList from of
              [Just label] | Set.size label == 1 && Set.findMin label /= home ->
                Just (Set.findMin label)

              _ ->
                Nothing
        }
  in
  Map.mapWithKey toEntry needs


-- Every import that is certainly loaded before this one, following parents.
ancestorsOf :: Map.Map ModuleName.Canonical (Maybe ModuleName.Canonical) -> ModuleName.Canonical -> Set.Set ModuleName.Canonical
ancestorsOf parents home =
  go Set.empty home
  where
    go acc h =
      case Map.findWithDefault Nothing h parents of
        Just p | Set.notMember p acc && p /= home ->
          go (Set.insert p acc) p

        _ ->
          acc



-- MERGE


data Group =
  Group
    { _gLabel :: Set.Set ModuleName.Canonical
    , _gGlobals :: Set.Set Opt.Global
    , _gSize :: Int
    , _gDeps :: Set.Set Int
      -- the other groups this one's code uses; the main bundle is not one
    }


data Target
  = IntoMain
  | Into Int


-- Repeatedly merge the smallest group that is under the minimum and has
-- somewhere to go. A group can always join the main bundle once
-- everything it uses is there, and can join another group unless that
-- would make two groups each wait for the other.
mergeAll
  :: Config
  -> Int
  -> (ModuleName.Canonical -> Set.Set ModuleName.Canonical)
  -> Set.Set Opt.Global
  -> Map.Map Int Group
  -> (Set.Set Opt.Global, Map.Map Int Group)
mergeAll config imports ancestors mainExtra groups =
  let
    small =
      List.sortOn (\(i, g) -> (_gSize g, i))
        [ (i, g) | (i, g) <- Map.toList groups, _gSize g < _minSize config ]

    choices =
      [ (i, target)
      | (i, g) <- small
      , Just target <- [ bestTarget config imports ancestors groups i g ]
      ]
  in
  case choices of
    [] ->
      (mainExtra, groups)

    (i, IntoMain) : _ ->
      let g = groups Map.! i in
      mergeAll config imports ancestors (Set.union mainExtra (_gGlobals g))
        (Map.map (\other -> other { _gDeps = Set.delete i (_gDeps other) }) (Map.delete i groups))

    (i, Into j) : _ ->
      mergeAll config imports ancestors mainExtra (mergeGroups i j groups)


bestTarget
  :: Config
  -> Int
  -> (ModuleName.Canonical -> Set.Set ModuleName.Canonical)
  -> Map.Map Int Group
  -> Int
  -> Group
  -> Maybe Target
bestTarget config imports ancestors groups i g =
  let
    notNeeding = imports - Set.size (_gLabel g)

    intoMain =
      [ (((notNeeding + _mainWeight config) * _gSize g, 0), IntoMain) | Set.null (_gDeps g) ]

    intoOthers =
      [ ((waste ancestors g h, j), Into j)
      | (j, h) <- Map.toList groups
      , j /= i
      , not (wouldCycle groups i j)
      ]
  in
  case List.sortOn fst (intoMain ++ intoOthers) of
    [] -> Nothing
    (_, target) : _ -> Just target


-- Bytes downloaded without being needed if g and h became one file: each
-- import that loads only one of them now fetches the other as well, unless
-- an import it waits on already brought that one in.
waste :: (ModuleName.Canonical -> Set.Set ModuleName.Canonical) -> Group -> Group -> Int
waste ancestors g h =
  let
    extra home other =
      if Set.null (Set.intersection (ancestors home) (_gLabel other)) then _gSize other else 0
  in
  sum [ extra home h | home <- Set.toList (Set.difference (_gLabel g) (_gLabel h)) ]
  + sum [ extra home g | home <- Set.toList (Set.difference (_gLabel h) (_gLabel g)) ]


-- Merging two groups closes a cycle when one reaches the other through a
-- third. The groups start out acyclic, since code only uses code whose
-- label contains its own, and every merge is checked here.
wouldCycle :: Map.Map Int Group -> Int -> Int -> Bool
wouldCycle groups i j =
  let
    depsOf k = maybe Set.empty _gDeps (Map.lookup k groups)
    reaches from to = reach Set.empty (Set.toList from) to
    reach seen todo to =
      case todo of
        [] -> False
        k : rest
          | k == to -> True
          | Set.member k seen -> reach seen rest to
          | otherwise -> reach (Set.insert k seen) (Set.toList (depsOf k) ++ rest) to
  in
  reaches (Set.delete j (depsOf i)) j || reaches (Set.delete i (depsOf j)) i


mergeGroups :: Int -> Int -> Map.Map Int Group -> Map.Map Int Group
mergeGroups i j groups =
  let
    g = groups Map.! i
    h = groups Map.! j
    merged =
      Group
        (Set.union (_gLabel g) (_gLabel h))
        (Set.union (_gGlobals g) (_gGlobals h))
        (_gSize g + _gSize h)
        (Set.delete i (Set.delete j (Set.union (_gDeps g) (_gDeps h))))
    rename other =
      if Set.member i (_gDeps other) then
        other { _gDeps = Set.insert j (Set.delete i (_gDeps other)) }
      else
        other
  in
  Map.insert j merged (Map.map rename (Map.delete i groups))



-- ORDER


topological :: Map.Map Int Group -> [Group]
topological groups =
  let
    visit (done, acc) i =
      if Set.member i done then
        (done, acc)
      else
        let
          (done1, acc1) =
            List.foldl' visit (Set.insert i done, acc) (Set.toList (_gDeps (groups Map.! i)))
        in
        (done1, groups Map.! i : acc1)

    (_, revOrder) =
      List.foldl' visit (Set.empty, []) (Map.keys groups)
  in
  reverse revOrder


toBundle :: Group -> Bundle
toBundle (Group label globals size _) =
  Bundle label globals size
