{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Tool.Package
  ( Documentation
  , decoder
  , summarize
  )
  where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import Numeric (readHex)

import qualified AST.Prim.Module as Module
import qualified AST.Source as Src
import qualified Data.Utf8 as Utf8
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Parse.Primitives as P
import qualified Parse.Type as Type
import Reporting.Doc ((<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Tool.Render as Render
import Tool.Summary (Summary(..), Entry(..))
import qualified Tool.Summary as S



-- DOCUMENTATION
--
-- The docs.json of a package, decoded here rather than with Elm.Docs because
-- that one drops the module from qualified type names, which `elm diff` does
-- not need but a reader does.


type Documentation =
  Map.Map Module.Name DModule


data DModule =
  DModule
    { _comment :: String
    , _unions :: [(String, String, [String], [(String, [Src.Type])])]
    , _aliases :: [(String, String, [String], Src.Type)]
    , _values :: [(String, String, Src.Type)]
    , _binops :: [(String, String, Src.Type)]
    }


decoder :: D.Decoder () Documentation
decoder =
  Map.fromList <$> D.list dModule


dModule :: D.Decoder () (Module.Name, DModule)
dModule =
  (,)
    <$> D.field "name" (Module.jsonDecodeName (\_ -> ()))
    <*> ( DModule
            <$> D.field "comment" string
            <*> D.field "unions" (D.list dUnion)
            <*> D.field "aliases" (D.list dAlias)
            <*> D.field "values" (D.list dValue)
            <*> D.field "binops" (D.list dValue)
        )


dUnion :: D.Decoder () (String, String, [String], [(String, [Src.Type])])
dUnion =
  (,,,)
    <$> D.field "name" string
    <*> D.field "comment" string
    <*> D.field "args" (D.list string)
    <*> D.field "cases" (D.list (D.pair string (D.list dType)))


dAlias :: D.Decoder () (String, String, [String], Src.Type)
dAlias =
  (,,,)
    <$> D.field "name" string
    <*> D.field "comment" string
    <*> D.field "args" (D.list string)
    <*> D.field "type" dType


dValue :: D.Decoder () (String, String, Src.Type)
dValue =
  (,,)
    <$> D.field "name" string
    <*> D.field "comment" string
    <*> D.field "type" dType


dType :: D.Decoder () Src.Type
dType =
  D.customString (P.specialize (\_ _ -> ()) (fst <$> Type.expression)) (\_ -> ())


string :: D.Decoder () String
string =
  unescape . Utf8.toChars <$> D.jsonString



-- SUMMARIZE
--
-- A module of a dependency. There are no line numbers, and everything in it
-- is exposed.


summarize :: Pkg.Name -> V.Version -> Module.Name -> DModule -> Summary
summarize pkg vsn name (DModule comment unions aliases values binops) =
  let
    home = Module.toChars name
    overview = nonEmpty comment

    entries =
      S.docsOrder overview $
        map (unionEntry home) unions
        ++ map (aliasEntry home) aliases
        ++ map (valueEntry home S.Value) values
        ++ map (valueEntry home S.Binop) binops
  in
  Summary
    { _module = home
    , _location = Pkg.toChars pkg ++ " " ++ V.toChars vsn
    , _exposing = "(" ++ List.intercalate ", " (map exposed entries) ++ ")"
    , _overview = overview
    , _entries = entries
    }


exposed :: Entry -> String
exposed e =
  case _kind e of
    S.Binop -> "(" ++ _name e ++ ")"
    S.Type | '=' `elem` _line e -> _name e ++ "(..)"
    _       -> _name e


entry :: String -> S.Kind -> D.Doc -> D.Doc -> String -> Entry
entry name kind line code comment =
  Entry name kind (Render.oneLine line) (Render.block code) (nonEmpty comment) True True Nothing


unionEntry :: String -> (String, String, [String], [(String, [Src.Type])]) -> Entry
unionEntry home (name, comment, vars, ctors) =
  let
    header = D.hsep ("type" : map D.fromChars (name : vars))
    toCtor (ctor, args) = RT.apply RT.None (D.fromChars ctor) (map (Render.public home RT.App) args)
    (line, code) =
      case map toCtor ctors of
        []   -> (header, header)
        c:cs ->
          ( D.hsep (header : ("=" <+> c) : map ("|" <+>) cs)
          , D.vcat [header, D.indent 4 (D.vcat (("=" <+> c) : map ("|" <+>) cs))]
          )
  in
  entry name S.Type line code comment


aliasEntry :: String -> (String, String, [String], Src.Type) -> Entry
aliasEntry home (name, comment, vars, t) =
  let
    header = D.hsep ("type" : "alias" : map D.fromChars (name : vars)) <+> "="
    body = Render.public home RT.None t
  in
  entry name S.Alias (header <+> body) (D.vcat [header, D.indent 4 body]) comment


valueEntry :: String -> S.Kind -> (String, String, Src.Type) -> Entry
valueEntry home kind (name, comment, t) =
  let
    shown = case kind of S.Binop -> "(" ++ name ++ ")"; _ -> name
    doc = Render.publicSignature home shown t
  in
  entry name kind doc doc comment



-- COMMENTS


nonEmpty :: String -> Maybe String
nonEmpty s =
  case List.dropWhileEnd Char.isSpace (dropWhile Char.isSpace s) of
    "" -> Nothing
    t  -> Just t


-- docs.json keeps strings with their JSON escapes.
unescape :: String -> String
unescape s =
  case s of
    '\\' : 'n' : rest -> '\n' : unescape rest
    '\\' : 't' : rest -> '\t' : unescape rest
    '\\' : 'r' : rest -> unescape rest
    '\\' : '"' : rest -> '"' : unescape rest
    '\\' : '/' : rest -> '/' : unescape rest
    '\\' : '\\' : rest -> '\\' : unescape rest
    '\\' : 'u' : a : b : c : d : rest | [(n, "")] <- readHex [a,b,c,d] -> Char.chr n : unescape rest
    c : rest -> c : unescape rest
    [] -> []

