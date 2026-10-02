{-# LANGUAGE OverloadedStrings #-}
module Tool.Sizes
  ( run
  , diff
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Directory as Dir
import Text.Printf (printf)

import qualified AST.Optimized as Opt
import qualified AST.Prim.Name as N
import qualified Data.Utf8 as Utf8
import qualified File
import qualified Json.Decode as D
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Tool.Async as Async
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project



-- RUN
--
--   elm tool sizes src/Main.elm
--   elm tool sizes src/Main.elm --json > before.json
--   elm tool sizes --diff before.json after.json
--
-- The bytes each module and each top level definition adds to an
-- --optimize build. The JSON is what --diff compares.


run :: FilePath -> IO (Either Problem Output)
run path =
  Project.withMain path $ \graph mains sizeOf ->
    case Map.toList mains of
      [(home, main)] ->
        let
          globals =
            List.sortOn (negate . snd)
              [ (globalToChars g, sizeOf g) | g <- Set.toList (Async.bundle graph home main) ]
          modules =
            List.sortOn (negate . snd) $ Map.toList $
              Map.fromListWith (+) [ (Async.moduleToChars m, sizeOf g) | g@(Opt.Global m _) <- Set.toList (Async.bundle graph home main) ]
        in
        return $ Right $ toOutput (Sizes (sum (map snd modules)) modules globals)

      _ ->
        return (Left (BadName "a file with a `main` in it" path))


globalToChars :: Opt.Global -> String
globalToChars (Opt.Global m name) =
  Async.moduleToChars m ++ "." ++ N.toChars name


data Sizes =
  Sizes
    { _total :: Int
    , _modules :: [(String, Int)]
    , _globals :: [(String, Int)]
    }


toOutput :: Sizes -> Output
toOutput (Sizes total modules globals) =
  let
    row (name, n) = printf "%9s  %s\n" (Async.kB n) name
  in
  Output
    ( "Bundle: " ++ Async.kB total ++ " kB (--optimize, before minification)\n\n"
      ++ "Modules:\n" ++ concatMap row modules
      ++ "\nLargest definitions:\n" ++ concatMap row (take 30 globals)
    )
    ( E.object
        [ "total" ==> E.int total
        , "modules" ==> E.list (\(n, s) -> E.object [ "name" ==> E.chars n, "size" ==> E.int s ]) modules
        , "globals" ==> E.list (\(n, s) -> E.object [ "name" ==> E.chars n, "size" ==> E.int s ]) globals
        ]
    )



-- DIFF


diff :: FilePath -> FilePath -> IO (Either Problem Output)
diff before after =
  do  b <- load before
      a <- load after
      return $
        case (b, a) of
          (Left p, _) -> Left p
          (_, Left p) -> Left p
          (Right old, Right new) -> Right (compareSizes old new)


load :: FilePath -> IO (Either Problem Sizes)
load path =
  do  exists <- Dir.doesFileExist path
      if not exists then return (Left (FileNotFound path)) else
        do  bytes <- File.readUtf8 path
            result <- D.fromByteString decoder bytes
            return $
              case result of
                Right sizes -> Right sizes
                Left _      -> Left (BadName "a file written by `elm tool sizes --json`" path)


decoder :: D.Decoder () Sizes
decoder =
  Sizes
    <$> D.field "total" D.int
    <*> D.field "modules" (D.list entry)
    <*> D.field "globals" (D.list entry)
  where
    entry = (,) <$> D.field "name" (Utf8.toChars <$> D.jsonString) <*> D.field "size" D.int


compareSizes :: Sizes -> Sizes -> Output
compareSizes (Sizes oldTotal oldModules oldGlobals) (Sizes newTotal newModules newGlobals) =
  let
    changes old new =
      List.sortOn (\(_, o, n) -> negate (abs (n - o))) $
        filter (\(_, o, n) -> o /= n)
          [ (name, Map.findWithDefault 0 name oldMap, Map.findWithDefault 0 name newMap)
          | let oldMap = Map.fromList old
          , let newMap = Map.fromList new
          , name <- Set.toList (Set.union (Map.keysSet oldMap) (Map.keysSet newMap))
          ]

    row (name, o, n) =
      printf "%+9.1f  %9s -> %-9s %s\n" (fromIntegral (n - o) / 1024 :: Double) (Async.kB o) (Async.kB n) name

    moduleChanges = changes oldModules newModules
    globalChanges = changes oldGlobals newGlobals

    toJson (name, o, n) = E.object [ "name" ==> E.chars name, "before" ==> E.int o, "after" ==> E.int n ]
  in
  Output
    ( printf "Bundle: %s kB -> %s kB (%+.1f kB)\n" (Async.kB oldTotal) (Async.kB newTotal) (fromIntegral (newTotal - oldTotal) / 1024 :: Double)
      ++ (if null moduleChanges then "\nNo module changed size.\n" else "\nModules:\n" ++ concatMap row moduleChanges)
      ++ (if null globalChanges then "" else "\nDefinitions:\n" ++ concatMap row (take 30 globalChanges))
    )
    ( E.object
        [ "before" ==> E.int oldTotal
        , "after" ==> E.int newTotal
        , "modules" ==> E.list toJson moduleChanges
        , "globals" ==> E.list toJson globalChanges
        ]
    )
