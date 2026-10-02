{-# LANGUAGE OverloadedStrings #-}
module Tool.Interface
  ( summarize
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map

import qualified String as S

import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Interface as I
import qualified Reporting.Annotation as A
import qualified Tool.Render as Render
import Tool.Summary (Summary(..), Entry(..))
import qualified Tool.Summary as Summary



-- SUMMARIZE
--
-- A package module that docs.json leaves out, which happens when its docs do
-- not check. The interface still has every exposed type, just no comments.


summarize :: String -> Module.Name -> I.Interface -> Summary
summarize location home (I.Interface _ values unions aliases _ binops _ _) =
  let
    ns = Render.names (asImportedBy home)

    entry name kind line code =
      Entry name kind line code Nothing True True Nothing

    unionEntry (name, iu) =
      case iu of
        I.OpenUnion u    -> Just (draw name Summary.Type (\l -> Render.union l ns name u True))
        I.ClosedUnion u  -> Just (draw name Summary.Type (\l -> Render.union l ns name u False))
        I.PrivateUnion _ -> Nothing

    aliasEntry (name, ia) =
      case ia of
        I.PublicAlias a  -> Just (draw name Summary.Alias (\l -> Render.alias l ns name a))
        I.PrivateAlias _ -> Nothing

    draw name kind toDoc =
      entry (T.nameToChars name) kind (Render.oneLine (toDoc Render.Inline)) (Render.block (toDoc Render.Declared))

    valueEntry (name, ann) =
      let doc = Render.signature ns name ann [] in
      entry (N.toChars name) Summary.Value (Render.oneLine doc) (Render.block doc)

    binopEntry (op, I.Binop _ ann _ _) =
      let doc = Render.signature ns (N.fromString (S.fromChars ("(" ++ Op.toChars op ++ ")"))) ann [] in
      entry (Op.toChars op) Summary.Binop (Render.oneLine doc) (Render.block doc)

    entries =
      List.sortOn _name [ e | Just e <- map unionEntry (Map.toList unions) ]
      ++ List.sortOn _name [ e | Just e <- map aliasEntry (Map.toList aliases) ]
      ++ List.sortOn _name (map valueEntry (Map.toList values))
      ++ List.sortOn _name (map binopEntry (Map.toList binops))

    exposed e =
      case _kind e of
        Summary.Binop -> "(" ++ _name e ++ ")"
        Summary.Type | '=' `elem` _line e -> _name e ++ "(..)"
        _ -> _name e
  in
  Summary
    { _module = Module.toChars home
    , _location = location
    , _exposing = "(" ++ List.intercalate ", " (map exposed entries) ++ ")"
    , _overview = Nothing
    , _entries = entries
    }


-- Types print as they would inside the module itself: its own names and the
-- default imports bare.
asImportedBy :: Module.Name -> Src.Module
asImportedBy home =
  Src.Module (Just (A.At A.zero home)) (A.At A.zero Src.Open) (Src.NoDocs A.zero [] [])
    Imports.defaults [] [] [] [] [] [] Src.NoEffects
