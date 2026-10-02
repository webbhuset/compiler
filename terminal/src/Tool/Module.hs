{-# LANGUAGE MagicHash #-}
module Tool.Module
  ( summarize
  )
  where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Build
import qualified Data.Utf8 as Utf8
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Tool.Render as Render
import Tool.Summary (Summary(..), Entry(..))
import qualified Tool.Summary as S



-- SUMMARIZE
--
-- A module of the project, after type checking. `everything` shows the
-- constructors of opaque types too.


summarize :: Bool -> FilePath -> Build.Checked -> IO Summary
summarize everything path (Build.Checked src can annotations _) =
  do  let (overview, valueDocs, typeDocs) = docsOf (Src._docs src)
      ov <- traverse comment overview
      entries <- traverse (toEntry everything src can annotations valueDocs typeDocs) (decls src)
      return $ Summary
        { _module = Module.toChars (Src.getName src)
        , _location = path
        , _exposing = exposingList (Can._exports can) entries
        , _overview = ov
        , _entries = entries
        }



-- DECLARATIONS


data Decl
  = Value A.Region N.Name (Maybe Src.Signature)
  | Union A.Region T.Name
  | Alias A.Region T.Name
  | Tag A.Region N.Name


-- In source order.
decls :: Src.Module -> [Decl]
decls (Src.Module _ _ _ _ values unions aliases tags _ _ _) =
  List.sortOn (\d -> start (region d)) $
    [ Value (valueRegion r sig) name sig | A.At r (Src.Value (A.At _ name) _ _ sig) <- values ]
    ++ [ Union r name | A.At r (Src.Union (A.At _ name) _ _) <- unions ]
    ++ [ Alias r name | A.At r (Src.Alias (A.At _ name) _ _) <- aliases ]
    ++ [ Tag r name | A.At r (Src.TagDecl (A.At _ name) _) <- tags ]


-- The region of a value starts at its body, so a signature above it is
-- folded in by hand.
valueRegion :: A.Region -> Maybe Src.Signature -> A.Region
valueRegion r sig =
  case sig of
    Just (Src.Signature (A.At typeRegion _) _) -> A.mergeRegions typeRegion r
    Nothing                                    -> r


region :: Decl -> A.Region
region decl =
  case decl of
    Value r _ _ -> r
    Union r _   -> r
    Alias r _   -> r
    Tag r _     -> r


start :: A.Region -> (Int, Int)
start (A.Region s _) =
  A.toEditorRowCol s


-- First and last line, counting from one.
lineRange :: A.Region -> (Int, Int)
lineRange (A.Region s e) =
  (fst (A.toEditorRowCol s), fst (A.toEditorRowCol e))



-- ENTRIES


toEntry
  :: Bool -> Src.Module -> Can.Module -> Map.Map N.Name Can.Annotation
  -> Map.Map N.Name Src.Comment -> Map.Map T.Name Src.Comment -> Decl -> IO Entry
toEntry everything src can annotations valueDocs typeDocs decl =
  let
    ns = Render.names src
    exports = Can._exports can

    entry name kind line code maybeComment exposed annotated =
      do  c <- traverse comment maybeComment
          return $ Entry name kind line code c exposed annotated (Just (lineRange (region decl)))
  in
  case decl of
    Value _ name sig ->
      let
        doc =
          case Map.lookup name annotations of
            Just ann ->
              let ann' = if Maybe.isJust sig then ann else Render.normalize ann in
              Render.signature ns name ann' (constraintsOf can name)

            Nothing ->
              D.fromName name
      in
      entry (N.toChars name) S.Value (Render.oneLine doc) (Render.block doc)
        (Map.lookup name valueDocs) (isExposedValue exports name) (Maybe.isJust sig)

    Union _ name ->
      let
        draw layout ctors =
          maybe ("type " ++ T.nameToChars name) id $
            (\u -> render layout (Render.union layout ns name u ctors)) <$> Map.lookup name (Can._unions can)
      in
      entry (T.nameToChars name) S.Type (draw Render.Inline True) (draw Render.Declared (everything || isOpen exports name))
        (Map.lookup name typeDocs) (isExposedType exports name) True

    Alias _ name ->
      let
        draw layout =
          maybe ("type alias " ++ T.nameToChars name) id $
            (\a -> render layout (Render.alias layout ns name a)) <$> Map.lookup name (Can._aliases can)
      in
      entry (T.nameToChars name) S.Alias (draw Render.Inline) (draw Render.Declared)
        (Map.lookup name typeDocs) (isExposedType exports name) True

    Tag _ name ->
      let
        line =
          maybe ("type tag " ++ N.toChars name) (Render.oneLine . Render.tag name) (Map.lookup name (Can._tags can))
      in
      entry (N.toChars name) S.Tag line line
        (Map.lookup name valueDocs) (isExposedType exports (T.nameFromName name)) True


render :: Render.Layout -> D.Doc -> String
render layout =
  case layout of
    Render.Inline   -> Render.oneLine
    Render.Declared -> Render.block


constraintsOf :: Can.Module -> N.Name -> [Can.Constraint]
constraintsOf can name =
  Map.findWithDefault [] (Can._name can, name) (Can._constrained (Can._overloads can))



-- EXPOSING


isExposedValue :: Can.Exports -> N.Name -> Bool
isExposedValue exports name =
  case exports of
    Can.ExportEverything _ -> True
    Can.Export _ values _  -> Map.member name values


isExposedType :: Can.Exports -> T.Name -> Bool
isExposedType exports name =
  case exports of
    Can.ExportEverything _ -> True
    Can.Export types _ _   -> Map.member name types


isOpen :: Can.Exports -> T.Name -> Bool
isOpen exports name =
  case exports of
    Can.ExportEverything _ -> True
    Can.Export types _ _   ->
      case Map.lookup name types of
        Just (_, Can.ExportUnionOpen) -> True
        _                             -> False


exposingList :: Can.Exports -> [Entry] -> String
exposingList exports entries =
  case exports of
    Can.ExportEverything _ ->
      "(..)"

    Can.Export types _ _ ->
      let
        toName e =
          case _kind e of
            S.Type | isOpenName (_name e) -> _name e ++ "(..)"
            _                             -> _name e

        isOpenName n =
          case [ t | (t, (_, Can.ExportUnionOpen)) <- Map.toList types, T.nameToChars t == n ] of
            [] -> False
            _  -> True
      in
      "(" ++ List.intercalate ", " [ toName e | e <- entries, _exposed e ] ++ ")"



-- COMMENTS


docsOf :: Src.Docs -> (Maybe Src.Comment, Map.Map N.Name Src.Comment, Map.Map T.Name Src.Comment)
docsOf docs =
  case docs of
    Src.NoDocs _ vs ts      -> (Nothing, Map.fromList vs, Map.fromList ts)
    Src.YesDocs ov vs ts    -> (Just ov, Map.fromList vs, Map.fromList ts)


comment :: Src.Comment -> IO String
comment (Src.Comment snippet) =
  trim . Utf8.toChars <$> (Utf8.fromSnippet snippet :: IO (Utf8.Utf8 ()))


trim :: String -> String
trim =
  List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace
