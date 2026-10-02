{-# LANGUAGE OverloadedStrings #-}
module Tool.Docs
  ( run
  )
  where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Json.Encode as E
import Json.Encode ((==>))
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import Tool.Summary (Summary(..), Entry(..))
import qualified Tool.Summary as S



-- RUN
--
--   elm tool docs Some.Module          the module comment and exposed API
--   elm tool docs Some.Module --all    private declarations too
--
-- The module comment is printed with each `@docs` line replaced by the
-- declarations it names, like the package website does. Exposed declarations
-- no `@docs` line names follow it.


run :: Bool -> String -> IO (Either Problem Output)
run everything target =
  case Project.parseModuleName target of
    Nothing ->
      return (Left (BadName "a module like `Page.Home`" target))

    Just home ->
      Project.withModule everything home $ \summary ->
        return $ Right $ Output
          (markdown everything summary)
          (json everything summary)



-- MARKDOWN


markdown :: Bool -> Summary -> String
markdown everything (Summary home _ _ overview entries) =
  let
    exposed = filter _exposed entries
    private = filter (not . _exposed) entries
    byName = Map.fromList [ (_name e, e) | e <- exposed ]

    (body, named) =
      case overview of
        Nothing -> ([], Set.empty)
        Just ov -> expand byName (demote (lines ov))

    rest = filter (\e -> Set.notMember (_name e) named) exposed

    section title es =
      if null es then [] else ["## " ++ title, ""] ++ concatMap entryToMarkdown es
  in
  unlines $ squeeze $
    ["# " ++ home, ""]
    ++ (if null body then [] else body ++ [""])
    ++ (if Set.null named then concatMap entryToMarkdown rest else section "Not in @docs" rest)
    ++ (if everything then section "Private" private else [])


-- Replace each `@docs a, b` line with the entries it names, and remember
-- which names were used.
expand :: Map.Map String Entry -> [String] -> ([String], Set.Set String)
expand byName =
  foldr step ([], Set.empty)
  where
    step line (out, named) =
      case List.stripPrefix "@docs" (dropWhile Char.isSpace line) of
        Just rest ->
          let
            docNames = map (stripParens . trim) (splitOn ',' rest)
            found = [ e | n <- docNames, Just e <- [Map.lookup n byName] ]
          in
          ("" : concatMap entryToMarkdown found ++ out, foldr Set.insert named docNames)

        Nothing ->
          (line : out, named)


entryToMarkdown :: Entry -> [String]
entryToMarkdown e =
  ["```elm"] ++ lines (_code e) ++ ["```", ""]
  ++ maybe [] (\c -> demote (lines c) ++ [""]) (_comment e)


-- Headings in a comment move one level down, under the `# Module` title.
demote :: [String] -> [String]
demote =
  go False
  where
    go inCode ls =
      case ls of
        [] -> []
        l:rest
          | "```" `List.isPrefixOf` l -> l : go (not inCode) rest
          | not inCode && "#" `List.isPrefixOf` l -> ('#' : l) : go inCode rest
          | otherwise -> l : go inCode rest


-- At most one blank line in a row.
squeeze :: [String] -> [String]
squeeze ls =
  case ls of
    a : b : rest | blank a && blank b -> squeeze (b : rest)
    a : rest                          -> a : squeeze rest
    []                                -> []
  where
    blank = all Char.isSpace



-- JSON


json :: Bool -> Summary -> E.Value
json everything (Summary home location _ overview entries) =
  E.object
    [ "module" ==> E.chars home
    , "location" ==> E.chars location
    , "comment" ==> maybe E.null E.chars overview
    , "declarations" ==> E.list entryToJson (filter (\e -> everything || _exposed e) entries)
    ]


entryToJson :: Entry -> E.Value
entryToJson e =
  E.object
    [ "name" ==> E.chars (_name e)
    , "kind" ==> E.chars (S.kindToChars (_kind e))
    , "code" ==> E.chars (_code e)
    , "comment" ==> maybe E.null E.chars (_comment e)
    , "exposed" ==> E.bool (_exposed e)
    ]



-- HELPERS


trim :: String -> String
trim =
  List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace


stripParens :: String -> String
stripParens s =
  case s of
    '(' : inner | not (null inner) && last inner == ')' -> init inner
    _                                                   -> s


splitOn :: Char -> String -> [String]
splitOn sep string =
  case break (== sep) string of
    (chunk, [])     -> [chunk]
    (chunk, _:rest) -> chunk : splitOn sep rest
