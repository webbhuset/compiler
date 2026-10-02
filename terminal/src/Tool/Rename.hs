module Tool.Rename
  ( run
  )
  where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Build
import qualified Json.Encode as E
import qualified Reporting.Annotation as A
import Tool.Edit (Edit(Edit))
import qualified Tool.Edit as Edit
import Tool.Index (Ref(..))
import qualified Tool.Index as Index
import Tool.Output (Output(..))
import Tool.Place (Place(..))
import qualified Tool.Place as Place
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import qualified Tool.Refs as Refs



-- RUN
--
--   elm tool rename Some.Module.old new
--   elm tool rename Some.Module.Old New --dry-run
--
-- Renames a value, type, or constructor everywhere in the project: its
-- definition and type annotation, every use, qualified or not, every
-- `exposing` list it is in, and the `@docs` lines of its module. Then the
-- project is checked again. A type and a constructor of the same name are
-- renamed together.


run :: Bool -> String -> String -> IO (Either Problem Output)
run dryRun target newName =
  case Refs.parseTarget target of
    Nothing ->
      return (Left (BadName "a value like `Page.Home.view` or a type like `Page.Home.Model`" target))

    Just (home, oldName)
      | not (validName oldName newName) ->
          return (Left (BadName ("a new name that starts with " ++ (if isUpper oldName then "an upper" else "a lower") ++ " case letter, like the old one, and is not a keyword") newName))

      | otherwise ->
          do  planned <-
                Project.withProject $ \root _ modules ->
                  case Refs.checkExists home oldName modules of
                    Just suggestions ->
                      return (Left (NotFound ("anything in " ++ Module.toChars home) oldName suggestions))

                    Nothing ->
                      case conflicts home newName modules of
                        Just why ->
                          return (Left (BadName why newName))

                        Nothing ->
                          if not (any ((== home) . Src.getName . Build._checked_source . snd) modules)
                            then return (Left (BadName "something defined in this project, not in a package" target))
                            else Right . (,) root <$> plan root home oldName newName (Index.index modules)

              case planned of
                Left problem ->
                  return (Left problem)

                Right (root, edits) ->
                  do  places <- Place.withSource root (Place.sortPlaces (map Edit.toPlace edits))
                      if dryRun
                        then return (Right (report "Would change" places Nothing))
                        else
                          do  Edit.apply root edits
                              checked <- Project.check []
                              return (Right (report "Changed" places (Just checked)))


report :: String -> [Place] -> Maybe (Either Problem Int) -> Output
report verb places checked =
  let
    files = List.nub (map _path places)
    summary =
      verb ++ " " ++ show (length places) ++ " places in " ++ show (length files) ++ " files.\n"
      ++ case checked of
           Nothing -> ""
           Just (Right n) -> "No errors in " ++ show n ++ " modules after the rename.\n"
           Just (Left _) -> "The project has errors after the rename. Run `elm tool check` to see them.\n"
  in
  Output (concatMap Place.toText places ++ "\n" ++ summary) (E.list Place.toJson places)



-- NAMES


isUpper :: String -> Bool
isUpper name =
  case name of
    c : _ -> Char.isUpper c
    []    -> False


validName :: String -> String -> Bool
validName old new =
  case new of
    c : cs ->
      isUpper old == Char.isUpper c
      && Char.isAlpha c
      && all (\x -> Char.isAlphaNum x || x == '_') cs
      && notElem new keywords

    [] ->
      False


keywords :: [String]
keywords =
  [ "if", "then", "else", "case", "of", "let", "in", "type", "module", "where"
  , "import", "exposing", "as", "port", "alias", "infix", "effect", "command", "subscription"
  ]


-- A rename that would make two things share a name.
conflicts :: Module.Name -> String -> [(FilePath, Build.Checked)] -> Maybe String
conflicts home newName modules =
  let
    taken = Maybe.fromMaybe [] (Index.namesIn home modules)
  in
  if elem newName taken
    then Just ("a name that " ++ Module.toChars home ++ " does not already have")
    else Nothing



-- PLAN


plan :: FilePath -> Module.Name -> String -> String -> [Index.Module] -> IO [Edit]
plan root home oldName newName modules =
  concat <$> traverse (planIn root home oldName newName) modules


planIn :: FilePath -> Module.Name -> String -> String -> Index.Module -> IO [Edit]
planIn root home oldName newName (Index.Module path checked refs) =
  let
    ours = [ r | r <- refs, _target r == (home, oldName), _kind r /= Index.Operator ]
    src = Build._checked_source checked
  in
  if null ours && Src.getName src /= home then return [] else
    do  ls <- Edit.readLines root path
        let lineAt n = Maybe.fromMaybe "" (lookup n (zip [1..] ls))
            editFor r =
              let
                (line, col) = Index.start (_region r)
                (_, endCol) = Index.end (_region r)
                (c1, c2) =
                  case _kind r of
                    Index.Match -> Edit.leadingName (lineAt line) col
                    _           -> Edit.lastSegmentAt (lineAt line) col endCol
              in
              Edit path line c1 c2 newName
            signatures =
              if Src.getName src == home then signatureEdits path ls src oldName newName else []
            docs =
              if Src.getName src == home then docsEdits path ls oldName newName else []
        return (map editFor ours ++ signatures ++ docs)


-- `update : Msg -> Model -> Model` above `update msg model =`. The source
-- AST keeps where the type is but not the name before it.
signatureEdits :: FilePath -> [String] -> Src.Module -> String -> String -> [Edit]
signatureEdits path ls src oldName newName =
  [ Edit path n 1 (1 + length oldName) newName
  | A.At _ (Src.Value (A.At _ v) _ _ (Just (Src.Signature (A.At typeRegion _) _))) <- Src._values src
  , N.toChars v == oldName
  , let typeLine = fst (Index.start typeRegion)
  , n <- take 1 [ k | k <- [typeLine, typeLine - 1 .. 1], isSignatureLine (Maybe.fromMaybe "" (lookup k (zip [1..] ls))) ]
  ]
  where
    isSignatureLine l =
      case List.stripPrefix oldName l of
        Just rest -> take 1 (dropWhile (== ' ') rest) == ":"
        Nothing   -> False


-- The name in `@docs a, b, c` lines.
docsEdits :: FilePath -> [String] -> String -> String -> [Edit]
docsEdits path ls oldName newName =
  [ Edit path n (col + 1) (col + 1 + length oldName) newName
  | (n, l) <- zip [1..] ls
  , "@docs" `List.isPrefixOf` dropWhile (== ' ') l
  , col <- wordColumns l oldName
  ]


-- Where a name stands as a whole word in a line, counting from zero.
wordColumns :: String -> String -> [Int]
wordColumns l word =
  [ i
  | i <- List.findIndices (const True) l
  , word `List.isPrefixOf` drop i l
  , i == 0 || not (isNameChar (l !! (i - 1)))
  , let after = drop (i + length word) l in null after || not (isNameChar (head' after))
  ]
  where
    isNameChar c = Char.isAlphaNum c || c == '_'
    head' s = case s of c : _ -> c; [] -> ' '

