{-# LANGUAGE OverloadedStrings #-}
module Tool.Chunks
  ( run
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf (printf)

import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName
import qualified Generate.ChunkGroups as Groups
import qualified Generate.Chunks as Chunks
import qualified Json.Encode as E
import Json.Encode ((==>))
import Tool.Async (kB, moduleToChars)
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project



-- RUN
--
--   elm tool chunks src/Main.elm
--
-- How the async imports of a program would be split into files if code
-- they share went into bundles of its own instead of into the main bundle
-- (Generate.ChunkGroups), next to how `elm make` splits them today. Sizes
-- are --optimize output before minification.


run :: FilePath -> IO (Either Problem Output)
run path =
  Project.withMain path $ \graph mains sizeOf ->
    case Map.toList mains of
      [_] ->
        case Chunks.plan False False graph mains of
          Left cycleNames ->
            return (Left (BadName ("a program without a cycle between " ++ List.intercalate ", " cycleNames) path))

          Right maybePlan ->
            let
              grouping = Groups.group Groups.defaultConfig sizeOf False False graph mains
            in
            return (Right (report sizeOf (today sizeOf (Chunks.discover False False graph mains) maybePlan) grouping))

      _ ->
        return (Left (BadName "a file with a `main` in it" path))



-- TODAY


data Today =
  Today
    { _mainSize :: Int
    , _files :: Map.Map ModuleName.Canonical Int
    }


-- Walked over the graph as the planners see it, without the debugger, as
-- an --optimize build leaves it out.
today :: (Opt.Global -> Int) -> Chunks.Discovery -> Maybe Chunks.Plan -> Today
today sizeOf (Chunks.Discovery nodes _ _) maybePlan =
  case maybePlan of
    Nothing ->
      Today 0 Map.empty

    Just (Chunks.Plan chunks mainSeen) ->
      Today (total sizeOf mainSeen) $ Map.fromList
        [ ( Chunks._home chunk
          , total sizeOf (Set.difference (Chunks.closure nodes Set.empty (Set.toList (Chunks._roots chunk))) mainSeen)
          )
        | chunk <- chunks
        ]


total :: (Opt.Global -> Int) -> Set.Set Opt.Global -> Int
total sizeOf globals =
  sum (map sizeOf (Set.toList globals))



-- REPORT


report :: (Opt.Global -> Int) -> Today -> Groups.Grouping -> Output
report sizeOf (Today mainToday files) grouping =
  let
    entries = Map.toList (Groups._entries grouping)
    bundles = zip [1 :: Int ..] (Groups._bundles grouping)
    mainGrouped = total sizeOf (Groups._main grouping)

    names homes = List.intercalate ", " (map moduleToChars (Set.toList homes))

    indexOf bundle =
      maybe 0 fst (List.find (\(_, b) -> Groups._label b == Groups._label bundle) bundles)

    needed e = total sizeOf (Set.difference (Groups._needs e) (Groups._main grouping))
    downloads home = sum (map Groups._size (Groups.fetches grouping home))

    bundleRow (i, b) =
      printf "  %3d %9s  %s\n" i (kB (Groups._size b)) (names (Groups._label b))

    entryRow (home, e) =
      printf "  %-32s %9s %9s  %-10s %9s\n"
        (moduleToChars home) (kB (needed e)) (kB (downloads home))
        (bundleList home)
        (maybe "in main" (\n -> if n == 0 then "in main" else kB n) (Map.lookup home files))

    bundleList home =
      case Groups.loads grouping home of
        [] -> "in main"
        bs -> List.intercalate "," (map (show . indexOf) bs)

    waterfalls =
      [ (home, e)
      | (home, e) <- entries
      , not (Groups._fromMain e)
      , not (Set.null (Groups._fromChunks e))
      , not (null (Groups.loads grouping home))
      ]

    -- a nested import costs no round trip when every bundle it loads is
    -- already loaded by an import that is certainly in before it
    folded home =
      null (Groups.fetches grouping home)

    waterfallRow (home, e) =
      "  " ++ moduleToChars home ++ " is fetched once "
      ++ maybe ("code in one of " ++ names (Groups._fromChunks e)) (\p -> moduleToChars p ++ "'s code") (Groups._parent e)
      ++ " asks for it: "
      ++ (if folded home then "its code is in that import's files already, no extra round trip.\n"
          else "one more round trip, unless fetched ahead of time.\n")
  in
  Output
    ( if null entries then "The program has no `import async`.\n" else
        "Main bundle: " ++ kB mainToday ++ " kB today, " ++ kB mainGrouped ++ " kB grouped"
        ++ " (--optimize, before minification)\n\n"
        ++ "Bundles, merged up to " ++ kB (Groups._minSize Groups.defaultConfig) ++ " kB:\n"
        ++ printf "  %3s %9s  %s\n" ("#" :: String) ("kB" :: String) ("loaded by" :: String)
        ++ concatMap bundleRow bundles
        ++ "\nEach async import:\n"
        ++ printf "  %-32s %9s %9s  %-10s %9s\n" ("import" :: String) ("needs" :: String) ("fetches" :: String) ("bundles" :: String) ("today" :: String)
        ++ concatMap entryRow entries
        ++ (if null waterfalls then "" else "\nNested imports:\n" ++ concatMap waterfallRow waterfalls)
    )
    ( E.object
        [ "main" ==> E.object [ "today" ==> E.int mainToday, "grouped" ==> E.int mainGrouped ]
        , "bundles" ==> E.list
            (\(i, b) -> E.object
                [ "id" ==> E.int i
                , "size" ==> E.int (Groups._size b)
                , "loadedBy" ==> E.list (E.chars . moduleToChars) (Set.toList (Groups._label b))
                ])
            bundles
        , "imports" ==> E.list
            (\(home, e) -> E.object
                [ "module" ==> E.chars (moduleToChars home)
                , "needs" ==> E.int (needed e)
                , "fetches" ==> E.int (downloads home)
                , "bundles" ==> E.list (E.int . indexOf) (Groups.loads grouping home)
                , "today" ==> maybe E.null E.int (Map.lookup home files)
                , "fromMain" ==> E.bool (Groups._fromMain e)
                , "fromImports" ==> E.list (E.chars . moduleToChars) (Set.toList (Groups._fromChunks e))
                , "waitsFor" ==> maybe E.null (E.chars . moduleToChars) (Groups._parent e)
                , "extraRoundTrip" ==> E.bool (not (Groups._fromMain e) && not (folded home))
                , "inMain" ==> E.bool (null (Groups.loads grouping home))
                ])
            entries
        ]
    )
