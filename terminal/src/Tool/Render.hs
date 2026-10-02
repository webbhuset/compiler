{-# LANGUAGE OverloadedStrings #-}
module Tool.Render
  ( Names
  , names
  , annotation
  , tipe
  , signature
  , Layout(..)
  , union
  , alias
  , tag
  , normalize
  , public
  , publicSignature
  , oneLine
  , block
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified String as S

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import Reporting.Doc ((<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L



-- NAMES
--
-- Types print the way the module they belong to would write them, so that a
-- signature can be pasted straight back into its source: `Html Msg` under
-- `import Html exposing (Html)`, `Html.Html Msg` without the exposing.


data Names =
  Names
    { _localizer :: L.Localizer
    , _prefixes :: Map.Map Module.Name String
    }


names :: Src.Module -> Names
names modul@(Src.Module _ _ _ imports _ _ _ _ _ _ _) =
  Names (L.fromModule modul) $ Map.fromList $
    [ (name, maybe (Module.toChars name) Module.prefixToChars as)
    | Src.Import (A.At _ name) as _ _ <- imports
    ]


-- How a `where` clause names the module that declares an overload.
prefix :: Names -> ModuleName.Canonical -> String
prefix (Names _ prefixes) (ModuleName.Canonical _ home) =
  Maybe.fromMaybe (Module.toChars home) (Map.lookup home prefixes)



-- SIGNATURES


-- `name : type`, followed by its `where` clauses if it has any.
signature :: Names -> N.Name -> Can.Annotation -> [Can.Constraint] -> D.Doc
signature ns name ann constraints =
  D.vcat $
    D.hang 4 (D.sep [D.fromName name <+> ":", annotation ns ann])
    : map (D.indent 4 . constraint ns) constraints


constraint :: Names -> Can.Constraint -> D.Doc
constraint ns (Can.Constraint (home, name) t) =
  "where" <+> D.fromChars (prefix ns home) <> "." <> D.fromName name <+> ":" <+> tipe ns RT.None t


annotation :: Names -> Can.Annotation -> D.Doc
annotation ns (Can.Forall _ t) =
  tipe ns RT.None t



-- TYPES


tipe :: Names -> RT.Context -> Can.Type -> D.Doc
tipe ns@(Names localizer _) context t =
  case t of
    Can.TLambda _ _ ->
      case map (tipe ns RT.Func) (collectLambdas t) of
        a:b:cs -> RT.lambda context a b cs
        _      -> error "Tool.Render: a lambda has at least two parts"

    Can.TVar x ->
      D.fromVar x

    Can.TType home name args ->
      RT.apply context (L.toDoc localizer home name) (map (tipe ns RT.App) args)

    Can.TRecord fields ext ->
      RT.record
        [ (D.fromName field, tipe ns RT.None fieldType) | (field, fieldType) <- fieldsInOrder fields ]
        (fmap D.fromVar ext)

    Can.TUnit ->
      "()"

    Can.TPair a b ->
      RT.tuple (tipe ns RT.None a) (tipe ns RT.None b) []

    Can.TTriple a b c ->
      RT.tuple (tipe ns RT.None a) (tipe ns RT.None b) [tipe ns RT.None c]

    Can.TAlias home name args _ ->
      RT.apply context (L.toDoc localizer home name) (map (tipe ns RT.App . snd) args)

    Can.TTagRow tags ext ->
      RT.tagRow
        [ RT.apply RT.None (L.toDoc localizer home (T.nameFromName tagName)) (map (tipe ns RT.App) args)
        | ((home, tagName), args) <- Map.toList tags
        ]
        (fmap D.fromVar ext)


-- Inferred records carry no source order, so they print alphabetically.
fieldsInOrder :: Map.Map N.Name Can.FieldType -> [(N.Name, Can.Type)]
fieldsInOrder fields =
  let
    entries = Map.toList fields
    index (_, Can.FieldType i _) = i
  in
  [ (name, t)
  | (name, Can.FieldType _ t) <-
      if all ((== 0) . index) entries
        then List.sortOn (N.toChars . fst) entries
        else List.sortOn index entries
  ]


collectLambdas :: Can.Type -> [Can.Type]
collectLambdas t =
  case t of
    Can.TLambda arg result -> arg : collectLambdas result
    _                      -> [t]



-- NORMALIZE
--
-- The solver names the variables of an inferred type in whatever order it
-- met them. Renaming them in order of appearance gives `a -> b -> a` rather
-- than `b -> a -> b`, keeping the `number` and `comparable` kinds.


normalize :: Can.Annotation -> Can.Annotation
normalize (Can.Forall free t) =
  let
    order = List.nub (varsOf t)
    renaming = Map.fromList (zip order (rename [] order))
    sub v = Map.findWithDefault v v renaming
  in
  Can.Forall (Map.mapKeys sub free) (substitute sub t)


rename :: [String] -> [T.Var] -> [T.Var]
rename taken vars =
  case vars of
    [] ->
      []

    v:vs ->
      let
        kind = List.find (`List.isPrefixOf` T.varToChars v) ["number", "comparable", "appendable", "compappend"]
        candidates =
          case kind of
            Just k  -> k : [ k ++ show n | n <- [(1::Int)..] ]
            Nothing -> letters
      in
      case filter (`notElem` taken) candidates of
        fresh : _ -> T.varFromString (S.fromChars fresh) : rename (fresh : taken) vs
        []        -> v : rename taken vs


letters :: [String]
letters =
  [ [c] | c <- ['a'..'z'] ] ++ [ c : show n | n <- [(1::Int)..], c <- ['a'..'z'] ]


varsOf :: Can.Type -> [T.Var]
varsOf t =
  case t of
    Can.TLambda a b        -> varsOf a ++ varsOf b
    Can.TVar x             -> [x]
    Can.TType _ _ args     -> concatMap varsOf args
    Can.TRecord fields ext -> maybe [] (:[]) ext ++ concatMap (varsOf . snd) (fieldsInOrder fields)
    Can.TUnit              -> []
    Can.TPair a b          -> varsOf a ++ varsOf b
    Can.TTriple a b c      -> varsOf a ++ varsOf b ++ varsOf c
    Can.TAlias _ _ args _  -> concatMap (varsOf . snd) args
    Can.TTagRow tags ext   -> concatMap (concatMap varsOf) (Map.elems tags) ++ maybe [] (:[]) ext


substitute :: (T.Var -> T.Var) -> Can.Type -> Can.Type
substitute sub t =
  let go = substitute sub in
  case t of
    Can.TLambda a b              -> Can.TLambda (go a) (go b)
    Can.TVar x                   -> Can.TVar (sub x)
    Can.TType home name args     -> Can.TType home name (map go args)
    Can.TRecord fields ext       -> Can.TRecord (Map.map (\(Can.FieldType i ft) -> Can.FieldType i (go ft)) fields) (fmap sub ext)
    Can.TUnit                    -> Can.TUnit
    Can.TPair a b                -> Can.TPair (go a) (go b)
    Can.TTriple a b c            -> Can.TTriple (go a) (go b) (go c)
    Can.TAlias home name args at -> Can.TAlias home name [ (sub v, go a) | (v, a) <- args ] (aliasType at)
    Can.TTagRow tags ext         -> Can.TTagRow (Map.map (map go) tags) (fmap sub ext)
  where
    aliasType at =
      case at of
        Can.Holey h  -> Can.Holey h
        Can.Filled f -> Can.Filled (substitute sub f)



-- DECLARATIONS


-- A custom type as it would be declared, or just `type Name` when it is
-- opaque to the reader.
--
-- With `Inline` everything goes on one line, for outlines. With `Declared`
-- it is laid out the way elm-format would write it.


data Layout = Inline | Declared


union :: Layout -> Names -> T.Name -> Can.Union -> Bool -> D.Doc
union layout ns name (Can.Union vars ctors _ _) showCtors =
  let
    header = D.hsep ("type" : D.fromType name : map D.fromVar vars)
    toCtor (Can.Ctor ctor _ _ args) = RT.apply RT.None (D.fromName ctor) (map (tipe ns RT.App) args)
  in
  case (showCtors, map toCtor ctors) of
    (True, c:cs) ->
      case layout of
        Inline   -> D.hsep (header : ("=" <+> c) : map ("|" <+>) cs)
        Declared -> D.vcat [header, D.indent 4 (D.vcat (("=" <+> c) : map ("|" <+>) cs))]

    _ ->
      header


alias :: Layout -> Names -> T.Name -> Can.Alias -> D.Doc
alias layout ns name (Can.Alias vars t) =
  let
    header = D.hsep ("type" : "alias" : D.fromType name : map D.fromVar vars) <+> "="
  in
  case layout of
    Inline   -> header <+> tipe ns RT.None t
    Declared -> D.vcat [header, D.indent 4 (tipe ns RT.None t)]



tag :: N.Name -> Can.TagDecl -> D.Doc
tag name (Can.TagDecl vars) =
  D.hsep ("type" : "tag" : D.fromName name : map D.fromVar vars)



-- PUBLIC TYPES
--
-- docs.json names every type in full, `Html.Html`. A name from the module
-- itself or from the default imports is printed bare, everything else keeps
-- its module, as it would be written after a plain `import Html`.


publicSignature :: String -> String -> Src.Type -> D.Doc
publicSignature home name t =
  D.hang 4 (D.sep [D.fromChars name <+> ":", public home RT.None t])


public :: String -> RT.Context -> Src.Type -> D.Doc
public home context (A.At _ t) =
  case t of
    Src.TLambda _ _ ->
      case map (public home RT.Func) (collectPublicLambdas (A.At A.zero t)) of
        a:b:cs -> RT.lambda context a b cs
        _      -> error "Tool.Render: a lambda has at least two parts"

    Src.TVar x ->
      D.fromVar x

    Src.TType _ name args ->
      RT.apply context (D.fromType name) (map (public home RT.App) args)

    Src.TTypeQual _ qualifier name args ->
      RT.apply context
        (D.fromChars (localize home (Module.prefixToChars qualifier ++ "." ++ T.nameToChars name)))
        (map (public home RT.App) args)

    Src.TRecord fields ext ->
      RT.record [ (D.fromName f, public home RT.None ft) | (A.At _ f, ft) <- fields ] (fmap (D.fromVar . A.toValue) ext)

    Src.TUnit ->
      "()"

    Src.TTuple a b cs ->
      RT.tuple (public home RT.None a) (public home RT.None b) (map (public home RT.None) cs)

    Src.TTagRow tags ext ->
      RT.tagRow
        [ RT.apply RT.None (D.fromChars (maybe (N.toChars tagName) (\q -> localize home (Module.prefixToChars q ++ "." ++ N.toChars tagName)) qualifier)) (map (public home RT.App) args)
        | Src.TagEntry _ qualifier tagName args <- tags
        ]
        (fmap (D.fromVar . A.toValue) ext)


collectPublicLambdas :: Src.Type -> [Src.Type]
collectPublicLambdas t =
  case A.toValue t of
    Src.TLambda arg result -> arg : collectPublicLambdas result
    _                      -> [t]


localize :: String -> String -> String
localize home qualified =
  let
    (revName, revHome) = break (== '.') (reverse qualified)
    name = reverse revName
    qualifier = reverse (drop 1 revHome)
  in
  if qualifier == home || qualifier == "Basics" || (qualifier, name) `elem` defaultTypes
    then name
    else
      case qualifier of
        "Platform.Cmd" -> "Cmd." ++ name
        "Platform.Sub" -> "Sub." ++ name
        _              -> qualified


defaultTypes :: [(String, String)]
defaultTypes =
  [ ("List", "List"), ("Maybe", "Maybe"), ("Result", "Result"), ("String", "String")
  , ("Char", "Char"), ("Platform", "Program"), ("Platform.Cmd", "Cmd"), ("Platform.Sub", "Sub")
  ]



-- OUTPUT


oneLine :: D.Doc -> String
oneLine =
  D.toLine


block :: D.Doc -> String
block =
  D.toString
