{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Optimize.Port
  ( toEncoder
  , toFlagsDecoder
  , toDecoder
  )
  where


import Prelude hiding (maybe, null)
import qualified Data.Map as Map

import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified AST.Utils.Type as Type
import qualified Data.Index as Index
import qualified Data.NonEmptyList as NE
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String as ES
import qualified Optimize.Names as Names



-- ENCODE


toEncoder :: Can.Type -> Names.Tracker Opt.Expr
toEncoder tipe =
  case tipe of
    Can.TAlias _ _ args alias ->
      toEncoder (Type.dealias args alias)

    Can.TLambda _ _ ->
      $(Crash.crash 'toEncoder) "function"

    Can.TVar _ ->
      $(Crash.crash 'toEncoder) "type variable"

    Can.TUnit ->
      Opt.Function [dollar] <$> encode [N.ascii|null|]

    Can.TTagRow _ _ ->
      $(Crash.crash 'toEncoder) "structural variants are not allowed through ports"

    Can.TPair a b ->
      encodePair a b

    Can.TTriple a b c ->
      encodeTriple a b c

    Can.TType _ name args ->
      case args of
        []
          | name == T.float  -> encode [N.ascii|float|]
          | name == T.int    -> encode [N.ascii|int|]
          | name == T.bool   -> encode [N.ascii|bool|]
          | name == T.string -> encode [N.ascii|string|]
          | name == T.value  -> Names.registerGlobal ModuleName.basics N.identity

        [arg]
          | name == T.maybe -> encodeMaybe arg
          | name == T.list  -> encodeList arg
          | name == T.array -> encodeArray arg

        _ ->
          $(Crash.crash 'toEncoder) "bad custom type"

    Can.TRecord _ (Just _) ->
      $(Crash.crash 'toEncoder) "bad record"

    Can.TRecord fields Nothing ->
      let
        encodeField (name, Can.FieldType _ fieldType) =
          do  encoder <- toEncoder fieldType
              let value = Opt.Call encoder [Opt.Access (Opt.VarLocal dollar) name]
              return $ Opt.Pair (Opt.Str (ES.fromName name)) value
      in
      do  object <- encode [N.ascii|object|]
          keyValuePairs <- traverse encodeField (Map.toList fields)
          Names.registerFieldDict fields $
            Opt.Function [dollar] (Opt.Call object [Opt.List keyValuePairs])



-- ENCODE HELPERS


encodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
encodeMaybe tipe =
  do  null <- encode [N.ascii|null|]
      encoder <- toEncoder tipe
      destruct <- Names.registerGlobal ModuleName.maybe [N.ascii|destruct|]
      return $ Opt.Function [dollar] $
        Opt.Call destruct [ null, encoder, Opt.VarLocal dollar ]


encodeList :: Can.Type -> Names.Tracker Opt.Expr
encodeList tipe =
  do  list <- encode [N.ascii|list|]
      encoder <- toEncoder tipe
      return $ Opt.Call list [ encoder ]


encodeArray :: Can.Type -> Names.Tracker Opt.Expr
encodeArray tipe =
  do  array <- encode [N.ascii|array|]
      encoder <- toEncoder tipe
      return $ Opt.Call array [ encoder ]


encodePair :: Can.Type -> Can.Type -> Names.Tracker Opt.Expr
encodePair a b =
  let
    let_ arg index body =
      Opt.Destruct (Opt.Destructor arg (Opt.Index index (Opt.Root dollar))) body

    encodeArg arg tipe =
      do  encoder <- toEncoder tipe
          return $ Opt.Call encoder [ Opt.VarLocal arg ]
  in
  do  list <- encode [N.ascii|list|]
      identity <- Names.registerGlobal ModuleName.basics N.identity
      arg1 <- encodeArg N.a a
      arg2 <- encodeArg N.b b
      return $ Opt.Function [dollar] $
        let_ N.a Index.first $
        let_ N.b Index.second $
          Opt.Call list [ identity, Opt.List [ arg1, arg2 ] ]


encodeTriple :: Can.Type -> Can.Type -> Can.Type -> Names.Tracker Opt.Expr
encodeTriple a b c =
  let
    let_ arg index body =
      Opt.Destruct (Opt.Destructor arg (Opt.Index index (Opt.Root dollar))) body

    encodeArg arg tipe =
      do  encoder <- toEncoder tipe
          return $ Opt.Call encoder [ Opt.VarLocal arg ]
  in
  do  list <- encode [N.ascii|list|]
      identity <- Names.registerGlobal ModuleName.basics N.identity
      arg1 <- encodeArg N.a a
      arg2 <- encodeArg N.b b
      arg3 <- encodeArg N.c c
      return $ Opt.Function [dollar] $
        let_ N.a Index.first $
        let_ N.b Index.second $
        let_ N.c Index.third $
          Opt.Call list [ identity, Opt.List [ arg1, arg2, arg3 ] ]


dollar :: N.Name
dollar = [N.ascii|$|]



-- FLAGS DECODER


toFlagsDecoder :: Can.Type -> Names.Tracker Opt.Expr
toFlagsDecoder tipe =
  case tipe of
    Can.TUnit ->
      do  succeed <- decode [N.ascii|succeed|]
          return $ Opt.Call succeed [ Opt.Unit ]

    _ ->
      toDecoder tipe



-- DECODE


toDecoder :: Can.Type -> Names.Tracker Opt.Expr
toDecoder tipe =
  case tipe of
    Can.TLambda _ _ ->
      $(Crash.crash 'toDecoder) "functions should not be allowed through input ports"

    Can.TVar _ ->
      $(Crash.crash 'toDecoder) "type variables should not be allowed through input ports"

    Can.TAlias _ _ args alias ->
      toDecoder (Type.dealias args alias)

    Can.TUnit ->
      decodeUnit

    Can.TTagRow _ _ ->
      $(Crash.crash 'toDecoder) "structural variants are not allowed through ports"

    Can.TPair a b ->
      decodePair a b

    Can.TTriple a b c ->
      decodeTriple a b c

    Can.TType _ name args ->
      case args of
        []
          | name == T.float  -> decode [N.ascii|float|]
          | name == T.int    -> decode [N.ascii|int|]
          | name == T.bool   -> decode [N.ascii|bool|]
          | name == T.string -> decode [N.ascii|string|]
          | name == T.value  -> decode [N.ascii|value|]

        [arg]
          | name == T.maybe -> decodeMaybe arg
          | name == T.list  -> decodeList arg
          | name == T.array -> decodeArray arg

        _ ->
          $(Crash.crash 'toDecoder) "bad type"

    Can.TRecord _ (Just _) ->
      $(Crash.crash 'toDecoder) "bad record"

    Can.TRecord fields Nothing ->
      decodeRecord fields



-- DECODE MAYBE


decodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
decodeMaybe tipe =
  do  nothing <- Names.registerGlobal ModuleName.maybe [N.ascii|Nothing|]
      just    <- Names.registerGlobal ModuleName.maybe [N.ascii|Just|]

      oneOf <- decode [N.ascii|oneOf|]
      null  <- decode [N.ascii|null|]
      map_  <- decode [N.ascii|map|]

      subDecoder <- toDecoder tipe

      return $
        Opt.Call oneOf
          [ Opt.List
              [ Opt.Call null [ nothing ]
              , Opt.Call map_ [ just, subDecoder ]
              ]
          ]


-- DECODE LIST


decodeList :: Can.Type -> Names.Tracker Opt.Expr
decodeList tipe =
  do  list <- decode [N.ascii|list|]
      decoder <- toDecoder tipe
      return $ Opt.Call list [ decoder ]



-- DECODE ARRAY


decodeArray :: Can.Type -> Names.Tracker Opt.Expr
decodeArray tipe =
  do  array <- decode [N.ascii|array|]
      decoder <- toDecoder tipe
      return $ Opt.Call array [ decoder ]



-- DECODE TUPLES


decodeUnit :: Names.Tracker Opt.Expr
decodeUnit =
  do  null <- decode [N.ascii|null|]
      return (Opt.Call null [ Opt.Unit ])


decodePair :: Can.Type -> Can.Type -> Names.Tracker Opt.Expr
decodePair a b =
  do  succeed <- decode [N.ascii|succeed|]
      indexAndThen 0 a =<<
        indexAndThen 1 b (Opt.Call succeed [Opt.Pair (toLocal 0) (toLocal 1)])


decodeTriple :: Can.Type -> Can.Type -> Can.Type -> Names.Tracker Opt.Expr
decodeTriple a b c =
  do  succeed <- decode [N.ascii|succeed|]
      indexAndThen 0 a =<<
        indexAndThen 1 b =<<
          indexAndThen 2 c (Opt.Call succeed [Opt.Triple (toLocal 0) (toLocal 1) (toLocal 2)])


toLocal :: Int -> Opt.Expr
toLocal index =
  Opt.VarLocal (Names.fromVarIndex index)


indexAndThen :: Int -> Can.Type -> Opt.Expr -> Names.Tracker Opt.Expr
indexAndThen i tipe decoder =
  do  andThen <- decode [N.ascii|andThen|]
      index <- decode [N.ascii|index|]
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call andThen
          [ Opt.Function [Names.fromVarIndex i] decoder
          , Opt.Call index [ Opt.Int i, typeDecoder ]
          ]



-- DECODE RECORDS


-- The field decoders are combined with map..map8 in a tree rather than
-- chained, so the generated JS nests log8(n) levels deep instead of one
-- function per field. Engines parse nested functions recursively, and iOS
-- gives up at a few hundred levels.
--
-- Fields are decoded last name first, so that a value with several bad
-- fields reports the same one as the earlier andThen chain did.
decodeRecord :: Map.Map N.Name Can.FieldType -> Names.Tracker Opt.Expr
decodeRecord fields =
  do  fieldParts <-
        Names.registerFieldDict fields =<<
          traverse fieldPart (reverse (Map.toList fields))
      case fieldParts of
        [] ->
          do  succeed <- decode [N.ascii|succeed|]
              return $ Opt.Call succeed [ Opt.Record Map.empty ]

        first : rest ->
          combineParts (NE.List first rest)


fieldPart :: (N.Name, Can.FieldType) -> Names.Tracker Part
fieldPart (key, Can.FieldType _ tipe) =
  do  field <- decode [N.ascii|field|]
      typeDecoder <- toDecoder tipe
      return $ Field key (Opt.Call field [ Opt.Str (ES.fromName key), typeDecoder ])


-- A node of the tree: a decoder for one or more of the record's fields.
data Part
  = Field N.Name Opt.Expr -- decodes the field's value
  | Fields [N.Name] Opt.Expr -- decodes a record of just these fields


-- Done once a single Fields part is left, which yields the whole record. A
-- lone Field still goes through one decodeGroup to wrap it in a record.
combineParts :: NE.List Part -> Names.Tracker Opt.Expr
combineParts parts =
  case parts of
    NE.List (Fields _ decoder) [] ->
      return decoder

    _ ->
      case chunksOfMapArity parts of
        NE.List onlyChunk [] ->
          combineParts . NE.singleton =<< decodeGroup onlyChunk

        chunks ->
          combineParts =<< traverse decodeGroupUnlessAlone chunks


-- A part left alone at the end of a round moves up to the next round as it
-- is: grouping it on its own would only copy its value into a new record.
decodeGroupUnlessAlone :: NE.List Part -> Names.Tracker Part
decodeGroupUnlessAlone chunk =
  case chunk of
    NE.List part [] ->
      return part

    NE.List _ (_ : _) ->
      decodeGroup chunk


-- The function given to mapN names its parameters _v0, _v1, … rather than
-- after the fields, because field names can be reserved words in JS (such
-- as `await` in an ES module).
decodeGroup :: NE.List Part -> Names.Tracker Part
decodeGroup parts =
  let
    partList =
      NE.toList parts

    args =
      zipWith (\i _ -> Names.fromVarIndex i) [0..] partList

    readFields arg part =
      case part of
        Field key _ ->
          [ (key, Opt.VarLocal arg) ]

        Fields keys _ ->
          [ (key, Opt.Access (Opt.VarLocal arg) key) | key <- keys ]

    partKeys part =
      case part of
        Field key _ -> [key]
        Fields keys _ -> keys

    partDecoder part =
      case part of
        Field _ decoder -> decoder
        Fields _ decoder -> decoder

    record =
      Opt.Record (Map.fromList (concat (zipWith readFields args partList)))
  in
  do  mapN <- decodeMapN (length parts)
      return $ Fields (concatMap partKeys partList) $
        Opt.Call mapN (Opt.Function args record : map partDecoder partList)


-- Splits into groups of 1 to 8, the arities of map..map8.
chunksOfMapArity :: NE.List a -> NE.List (NE.List a)
chunksOfMapArity (NE.List first rest) =
  let
    (chunkRest, later) =
      splitAt 7 rest

    chunk =
      NE.List first chunkRest
  in
  case later of
    [] ->
      NE.List chunk []

    next : afterNext ->
      NE.List chunk (NE.toList (chunksOfMapArity (NE.List next afterNext)))


-- Takes the length of a group from chunksOfMapArity.
decodeMapN :: Int -> Names.Tracker Opt.Expr
decodeMapN arity =
  case arity of
    1 -> decode [N.ascii|map|]
    2 -> decode [N.ascii|map2|]
    3 -> decode [N.ascii|map3|]
    4 -> decode [N.ascii|map4|]
    5 -> decode [N.ascii|map5|]
    6 -> decode [N.ascii|map6|]
    7 -> decode [N.ascii|map7|]
    8 -> decode [N.ascii|map8|]
    _ -> $(Crash.crash 'decodeMapN) ("no Json.Decode.map with arity " ++ show arity)



-- GLOBALS HELPERS


encode :: N.Name -> Names.Tracker Opt.Expr
encode name =
  Names.registerGlobal ModuleName.jsonEncode name


decode :: N.Name -> Names.Tracker Opt.Expr
decode name =
  Names.registerGlobal ModuleName.jsonDecode name
