{-# LANGUAGE PatternGuards #-}
module Tool.Move
  ( run
  )
  where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Build
import qualified Json.Encode as E
import qualified Reporting.Annotation as A
import Tool.Edit (Edit(Edit, Span))
import qualified Tool.Edit as Edit
import Tool.Index (Ref(..))
import qualified Tool.Index as Index
import Tool.Output (Output(..))
import qualified Tool.Place as Place
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project



-- RUN
--
--   elm tool move Some.Module.fn Other.Module
--   elm tool move Some.Module.fn Other.Module --dry-run
--
-- Moves a top level definition, with its doc comment and annotation, to the
-- end of another module of the project, and fixes the imports: the target
-- gets the imports the definition needs, every module that used it is
-- pointed at its new home, and the old module imports it back if it still
-- uses it. Then the project is checked again.
--
-- A definition that uses other definitions of its module has to have those
-- moved first, and a move that would make modules import each other is
-- refused.


run :: Bool -> String -> String -> IO (Either Problem Output)
run dryRun target destination =
  case (Project.parseValueName target, Project.parseModuleName destination) of
    (Nothing, _) ->
      return (Left (BadName "a value like `Page.Home.viewHeader`" target))

    (_, Nothing) ->
      return (Left (BadName "a module like `View.Header`" destination))

    (Just (home, name), Just to)
      | home == to ->
          return (Left (BadName "a module other than the one it is in" destination))

      | otherwise ->
          do  planned <-
                Project.withProject $ \root _ modules ->
                  case plan home (N.toChars name) to (Index.index modules) of
                    Left problem -> return (Left problem)
                    Right make ->
                      do  (edits, warnings) <- make root
                          return (Right (root, edits, warnings))

              case planned of
                Left problem ->
                  return (Left problem)

                Right (root, edits, warnings) ->
                  do  places <- Place.withSource root (Place.sortPlaces (map Edit.toPlace edits))
                      let warningText = concatMap (\w -> "Warning: " ++ w ++ "\n") warnings
                      if dryRun
                        then return (Right (report places ("Would change " ++ summary places ++ warningText)))
                        else
                          do  Edit.apply root edits
                              checked <- Project.check []
                              return $ Right $ report places $
                                "Changed " ++ summary places ++ warningText
                                ++ case checked of
                                     Right n -> "No errors in " ++ show n ++ " modules after the move.\n"
                                     Left _  -> "The project has errors after the move. Run `elm tool check` to see them.\n"


summary :: [Place.Place] -> String
summary places =
  show (length places) ++ " places in " ++ show (length (List.nub (map Place._path places))) ++ " files.\n"


report :: [Place.Place] -> String -> Output
report places footer =
  Output (concatMap Place.toText places ++ "\n" ++ footer) (E.list Place.toJson places)



-- PLAN


type Planner =
  FilePath -> IO ([Edit], [String])


plan :: Module.Name -> String -> Module.Name -> [Index.Module] -> Either Problem Planner
plan home name to modules =
  let
    byName = Map.fromList [ (moduleName m, m) | m <- modules ]
    moduleName m = Src.getName (Build._checked_source (Index._checked m))
    sourceOf m = Build._checked_source (Index._checked m)
  in
  case (Map.lookup home byName, Map.lookup to byName) of
    (Nothing, _) -> Left (BadName "a definition in this project" (Module.toChars home ++ "." ++ name))
    (_, Nothing) -> Left (BadName "a module of this project" (Module.toChars to))
    (Just from, Just dest) ->
      case [ v | A.At _ v@(Src.Value (A.At _ n) _ _ _) <- Src._values (sourceOf from), N.toChars n == name ] of
        [] -> Left (NotFound ("a top level value in " ++ Module.toChars home) name [])
        _ | name == "main" -> Left (BadName "something other than `main`" name)
        _ ->
          let
            refsIn m = Index._refs m
            uses r = _kind r `notElem` [Index.Definition, Index.Exposing]
            own = [ r | r <- refsIn from, _in r == Just name, uses r ]
            needsOwn = List.nub [ n | r <- own, (m, n) <- [_target r], m == home, n /= name ]
            needsDest = [ r | r <- own, fst (_target r) == to ]
            needed = List.nub [ m | r <- own, let m = fst (_target r), m /= home, m /= to ]

            importsOf m = maybe [] (map Src.getImportName . Src._imports . sourceOf) (Map.lookup m byName)
            dependsOn a b = Set.member b (reach importsOf [a])

            usedInHome = any (\r -> _target r == (home, name) && _in r /= Just name && uses r) (refsIn from)
            users = [ m | m <- modules, moduleName m `notElem` [home, to], any (\r -> _target r == (home, name) && uses r) (refsIn m) ]
          in
          if not (null needsOwn) then
            Left (CannotMove name ("it uses " ++ List.intercalate ", " needsOwn ++ " from " ++ Module.toChars home ++ ", so move those first"))
          else if not (null needsDest) then
            Left (CannotMove name ("it uses " ++ Module.toChars to ++ " itself"))
          else
            case [ m | m <- needed, Map.member m byName, dependsOn m to ] of
              m : _ -> Left (CannotMove name (Module.toChars m ++ ", which it uses, imports " ++ Module.toChars to ++ ", so the two would import each other"))
              [] | usedInHome && dependsOn to home ->
                     Left (CannotMove name (Module.toChars to ++ " imports " ++ Module.toChars home ++ ", which still uses it, so the two would import each other"))
              [] ->
                case [ moduleName u | u <- users, dependsOn to (moduleName u) ] of
                  u : _ -> Left (CannotMove name (Module.toChars to ++ " imports " ++ Module.toChars u ++ ", which uses it, so the two would import each other"))
                  [] -> Right (\root -> planEdits root home name to from dest needed usedInHome users)


reach :: (Module.Name -> [Module.Name]) -> [Module.Name] -> Set.Set Module.Name
reach next =
  go Set.empty
  where
    go seen todo =
      case todo of
        [] -> seen
        m : rest
          | Set.member m seen -> go seen rest
          | otherwise -> go (Set.insert m seen) (next m ++ rest)



-- EDITS


planEdits :: FilePath -> Module.Name -> String -> Module.Name -> Index.Module -> Index.Module -> [Module.Name] -> Bool -> [Index.Module] -> IO ([Edit], [String])
planEdits root home name to from dest needed usedInHome users =
  do  fromLines <- Edit.readLines root (Index._path from)
      destLines <- Edit.readLines root (Index._path dest)
      userLines <- traverse (\u -> (,) u <$> Edit.readLines root (Index._path u)) users

      let fromSrc = Build._checked_source (Index._checked from)
          destSrc = Build._checked_source (Index._checked dest)
          fromPath = Index._path from
          destPath = Index._path dest
          target = (home, name)

          (blockStart, blockEnd) = block fromLines fromSrc name
          blockText = take (blockEnd - blockStart + 1) (drop (blockStart - 1) fromLines)
          (delStart, delEnd) = withBlankLines fromLines blockStart blockEnd

          -- the module the definition leaves
          removeBlock = Span fromPath delStart 1 (delEnd + 1) 1 ""
          ownExposing = [ removeFromList fromPath fromLines r | r <- Index._refs from, _target r == target, _kind r == Index.Exposing ]
          ownDocs = docsRemovals fromPath fromLines name
          importBack =
            if not usedInHome then [] else
              exposeFrom fromPath fromLines fromSrc to name

          -- the module it arrives in
          wasExposed = not (null ownExposing)
          needsExposing = wasExposed || usedInHome || not (null users)
          append = Span destPath (length destLines + 1) 1 (length destLines + 1) 1 ("\n\n" ++ List.intercalate "\n" blockText)
          newImports =
            [ line
            | m <- needed
            , Src.Import (A.At r m') _ _ _ <- Src._imports fromSrc
            , m' == m, not (isImplicit r)
            , not (importsExplicitly destSrc m)
            , let line = Maybe.fromMaybe "" (lineAt fromLines (fst (Index.start r)))
            ]
          importEdits = insertImports destPath destLines destSrc newImports
          exposeInDest = if needsExposing then addToModuleExposing destPath destLines destSrc name else []
          destRefs = [ r | r <- Index._refs dest, _target r == target ]
          localizeInDest =
            [ e | r <- destRefs, _kind r /= Index.Exposing, Just e <- [dropQualifier destPath destLines r] ]
            ++ ( if onlyFor dest home name
                   then dropImport destPath destSrc home
                   else [ removeFromList destPath destLines r | r <- destRefs, _kind r == Index.Exposing ] )

          -- every other module that used it
          userEdits = concat [ repoint u ls to target | (u, ls) <- userLines ]

          ownRefs = [ r | r <- Index._refs from, _in r == Just name, _kind r `notElem` [Index.Definition, Index.Exposing] ]
          warnings =
            [ "the definition writes " ++ Module.toChars m ++ " under another name than " ++ Module.toChars to ++ " imports it as, so check the qualified names in it"
            | m <- needed
            , differentQualifier fromSrc destSrc m
            , any (\r -> fst (_target r) == m && isQualified fromLines r) ownRefs
            ]
      return
        ( [removeBlock] ++ ownExposing ++ ownDocs ++ importBack
          ++ [append] ++ importEdits ++ exposeInDest ++ localizeInDest
          ++ userEdits
        , warnings
        )


lineAt :: [String] -> Int -> Maybe String
lineAt ls n =
  case drop (n - 1) ls of
    l : _ | n >= 1 -> Just l
    _              -> Nothing


isImplicit :: A.Region -> Bool
isImplicit region =
  Index.start region == Index.end region


importsExplicitly :: Src.Module -> Module.Name -> Bool
importsExplicitly src m =
  or [ m' == m && not (isImplicit r) | Src.Import (A.At r m') _ _ _ <- Src._imports src ]


qualifierFor :: Src.Module -> Module.Name -> Maybe String
qualifierFor src m =
  Maybe.listToMaybe
    [ maybe (Module.toChars m) Module.prefixToChars alias
    | Src.Import (A.At _ m') alias _ _ <- Src._imports src, m' == m
    ]


differentQualifier :: Src.Module -> Src.Module -> Module.Name -> Bool
differentQualifier a b m =
  case (qualifierFor a m, qualifierFor b m) of
    (Just x, Just y) -> x /= y
    _                -> False



-- THE BLOCK


-- The lines of a definition: its doc comment, annotation, and body.
block :: [String] -> Src.Module -> String -> (Int, Int)
block ls src name =
  let
    found =
      [ (start, fst (Index.end r))
      | A.At r (Src.Value (A.At nameRegion n) _ _ sig) <- Src._values src
      , N.toChars n == name
      , let defLine = fst (Index.start nameRegion)
      , let start = maybe defLine (signatureLine defLine) sig
      ]
    signatureLine defLine (Src.Signature (A.At typeRegion _) _) =
      let typeLine = fst (Index.start typeRegion) in
      Maybe.fromMaybe defLine $ Maybe.listToMaybe
        [ k | k <- [typeLine, typeLine - 1 .. 1], isSignatureLine (Maybe.fromMaybe "" (lineAt ls k)) ]
    isSignatureLine l =
      case List.stripPrefix name l of
        Just rest -> take 1 (dropWhile (== ' ') rest) == ":"
        Nothing   -> False
  in
  case found of
    (start, end) : _ -> (docStart ls start, end)
    []               -> (1, 0)


-- Step up over a doc comment that ends right above a line.
docStart :: [String] -> Int -> Int
docStart ls start =
  case lineAt ls (start - 1) of
    Just l | "-}" `List.isSuffixOf` trimEnd l ->
      Maybe.fromMaybe start $ Maybe.listToMaybe
        [ k | k <- [start - 1, start - 2 .. 1], "{-|" `List.isPrefixOf` Maybe.fromMaybe "" (lineAt ls k) ]

    _ ->
      start


-- Take the blank lines after a block with it, or the ones before it when it
-- is the last thing in the file, so the gap between what is left stays the
-- same.
withBlankLines :: [String] -> Int -> Int -> (Int, Int)
withBlankLines ls start end =
  let
    blank k = maybe False (all Char.isSpace) (lineAt ls k)
    after = length (takeWhile blank [end + 1 .. length ls])
    before = length (takeWhile blank [start - 1, start - 2 .. 1])
  in
  if end + after >= length ls
    then (start - before, length ls)
    else (start, end + after)


trimEnd :: String -> String
trimEnd =
  List.dropWhileEnd Char.isSpace



-- EXPOSING LISTS


-- Take a name out of an exposing list, with the comma next to it. When it
-- is the only one in an import, the `exposing` goes.
removeFromList :: FilePath -> [String] -> Ref -> Edit
removeFromList path ls (Ref _ region _ _) =
  let
    (line, col) = Index.start region
    (_, endCol) = Index.end region
  in
  removeAt path ls line col endCol


removeAt :: FilePath -> [String] -> Int -> Int -> Int -> Edit
removeAt path ls line col endCol =
  let
    l = Maybe.fromMaybe "" (lineAt ls line)
    after = drop (endCol - 1) l
    before = take (col - 1) l
    afterSpaces = length (takeWhile (== ' ') after)
    beforeTrim = trimEnd before
  in
  if take 1 (drop afterSpaces after) == "," then
    let rest = drop (afterSpaces + 1) after in
    Edit path line col (endCol + afterSpaces + 1 + length (takeWhile (== ' ') rest)) ""
  else if not (null beforeTrim) && last beforeTrim == ',' then
    Edit path line (length beforeTrim) endCol ""
  else
    case (stripSuffix "(" beforeTrim, dropWhile (== ' ') after) of
      (Just upToParen, ')' : _) | Just withoutExposing <- stripSuffix "exposing" (trimEnd upToParen) ->
        Edit path line (length (trimEnd withoutExposing) + 1) (endCol + afterSpaces + 1) ""
      _ ->
        Edit path line col endCol ""


stripSuffix :: String -> String -> Maybe String
stripSuffix suffix s =
  reverse <$> List.stripPrefix (reverse suffix) (reverse s)


-- Add a name to a module's own exposing list.
addToModuleExposing :: FilePath -> [String] -> Src.Module -> String -> [Edit]
addToModuleExposing path ls src name =
  case Src._exports src of
    A.At _ Src.Open -> []
    A.At region (Src.Explicit _) ->
      let
        (endLine, endCol) = Index.end region
        l = Maybe.fromMaybe "" (lineAt ls endLine)
        paren = endCol - 1
        beforeParen = take (paren - 1) l
      in
      if all Char.isSpace beforeParen && endLine > 1
        then
          let indent = takeWhile (== ' ') l in
          [ Span path endLine 1 endLine 1 (indent ++ ", " ++ name ++ "\n") ]
        else
          [ Edit path endLine paren paren (", " ++ name) ]


-- Make a module see `name` from `m` unqualified: add it to an import that
-- has an exposing list, or add an import.
exposeFrom :: FilePath -> [String] -> Src.Module -> Module.Name -> String -> [Edit]
exposeFrom path ls src m name =
  case [ (r, exposing) | Src.Import (A.At r m') _ exposing _ <- Src._imports src, m' == m, not (isImplicit r) ] of
    (_, Src.Open) : _ ->
      []

    (r, Src.Explicit _) : _ ->
      let
        line = fst (Index.start r)
        l = Maybe.fromMaybe "" (lineAt ls line)
      in
      case List.elemIndices ')' l of
        [] -> [ Edit path line (length l + 1) (length l + 1) (" exposing (" ++ name ++ ")") ]
        ps ->
          if "exposing" `List.isInfixOf` l
            then [ Edit path line (last ps + 1) (last ps + 1) (", " ++ name) ]
            else [ Edit path line (length l + 1) (length l + 1) (" exposing (" ++ name ++ ")") ]

    [] ->
      insertImports path ls src ["import " ++ Module.toChars m ++ " exposing (" ++ name ++ ")"]


-- Put import lines after the last import, or after the module header and
-- its comment when there are none.
insertImports :: FilePath -> [String] -> Src.Module -> [String] -> [Edit]
insertImports path ls src imports =
  if null imports then [] else
    let
      explicit = [ fst (Index.end r) | Src.Import (A.At r _) _ _ _ <- Src._imports src, not (isImplicit r) ]
    in
    case explicit of
      _ : _ ->
        let after = maximum explicit + 1 in
        [ Span path after 1 after 1 (concatMap (++ "\n") imports) ]

      [] ->
        let
          A.At headerRegion _ = Src._exports src
          headerEnd = fst (Index.end headerRegion)
          afterComment = commentEnd ls (headerEnd + 1)
        in
        [ Span path (afterComment + 1) 1 (afterComment + 1) 1 ("\n" ++ concatMap (++ "\n") imports) ]


-- The last line of a doc comment starting at or after a line, skipping blank
-- lines, or the line before when there is none.
commentEnd :: [String] -> Int -> Int
commentEnd ls from =
  let
    blank k = maybe False (all Char.isSpace) (lineAt ls k)
    first = head' (dropWhile blank [from .. length ls]) (from - 1)
  in
  if maybe False ("{-|" `List.isPrefixOf`) (lineAt ls first)
    then head' [ k | k <- [first .. length ls], maybe False (("-}" `List.isSuffixOf`) . trimEnd) (lineAt ls k) ] (from - 1)
    else from - 1
  where
    head' xs d = case xs of x : _ -> x; [] -> d


-- The name in the module's `@docs` lines, with its comma.
docsRemovals :: FilePath -> [String] -> String -> [Edit]
docsRemovals path ls name =
  [ e
  | (n, l) <- zip [1..] ls
  , "@docs" `List.isPrefixOf` dropWhile (== ' ') l
  , col <- wordColumns l name
  , let e = removeAt path ls n (col + 1) (col + 1 + length name)
  ]


-- Point the uses in another module at the new home.
repoint :: Index.Module -> [String] -> Module.Name -> (Module.Name, String) -> [Edit]
repoint user ls to target@(_, name) =
  let
    path = Index._path user
    src = Build._checked_source (Index._checked user)
    refs = [ r | r <- Index._refs user, _target r == target ]
    uses = [ r | r <- refs, _kind r /= Index.Exposing ]
    qualified = [ r | r <- uses, isQualified ls r ]
    unqualified = [ r | r <- uses, not (isQualified ls r) ]
    destQualifier = Maybe.fromMaybe (Module.toChars to) (qualifierFor src to)
    importsDest = importsExplicitly src to
  in
  [ e | r <- qualified, Just e <- [replaceQualifier path ls r destQualifier] ]
  ++ ( if onlyFor user (fst target) name
         then dropImport path src (fst target)
         else [ removeFromList path ls r | r <- refs, _kind r == Index.Exposing ] )
  ++ ( if not (null unqualified) then exposeFrom path ls src to name
       else if not importsDest then insertImports path ls src ["import " ++ Module.toChars to]
       else [] )


-- Whether a module imports another only for this one name.
onlyFor :: Index.Module -> Module.Name -> String -> Bool
onlyFor m home name =
  null [ r | r <- Index._refs m, fst (_target r) == home, snd (_target r) /= name ]


-- Delete an import line.
dropImport :: FilePath -> Src.Module -> Module.Name -> [Edit]
dropImport path src m =
  [ Span path line 1 (line + 1) 1 ""
  | Src.Import (A.At r m') _ _ _ <- Src._imports src, m' == m, not (isImplicit r)
  , let line = fst (Index.start r)
  ]


isQualified :: [String] -> Ref -> Bool
isQualified ls (Ref _ region _ _) =
  let
    (line, col) = Index.start region
    (_, endCol) = Index.end region
    l = Maybe.fromMaybe "" (lineAt ls line)
  in
  fst (Edit.lastSegmentAt l col endCol) > col


replaceQualifier :: FilePath -> [String] -> Ref -> String -> Maybe Edit
replaceQualifier path ls (Ref _ region _ _) qualifier =
  let
    (line, col) = Index.start region
    (_, endCol) = Index.end region
    l = Maybe.fromMaybe "" (lineAt ls line)
    (nameStart, _) = Edit.lastSegmentAt l col endCol
  in
  if nameStart > col then Just (Edit path line col (nameStart - 1) qualifier) else Nothing


-- In the module the definition arrives in, `Old.name` becomes `name`.
dropQualifier :: FilePath -> [String] -> Ref -> Maybe Edit
dropQualifier path ls (Ref _ region _ _) =
  let
    (line, col) = Index.start region
    (_, endCol) = Index.end region
    l = Maybe.fromMaybe "" (lineAt ls line)
    (nameStart, _) = Edit.lastSegmentAt l col endCol
  in
  if nameStart > col then Just (Edit path line col nameStart "") else Nothing



-- HELPERS


wordColumns :: String -> String -> [Int]
wordColumns l word =
  [ i
  | i <- [0 .. length l - 1]
  , word `List.isPrefixOf` drop i l
  , i == 0 || not (isNameChar (l !! (i - 1)))
  , case drop (i + length word) l of { c : _ -> not (isNameChar c); [] -> True }
  ]
  where
    isNameChar c = Char.isAlphaNum c || c == '_'
