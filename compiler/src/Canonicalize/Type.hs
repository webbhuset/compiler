{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Type
  ( toAnnotation
  , canonicalize
  )
  where


import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a



-- TO ANNOTATION


toAnnotation :: Env.Env -> Src.Type -> Result i w Can.Annotation
toAnnotation env srcType =
  do  tipe <- canonicalize env srcType
      Result.ok $ Can.Forall (addFreeVars Map.empty tipe) tipe



-- CANONICALIZE TYPES


canonicalize :: Env.Env -> Src.Type -> Result i w Can.Type
canonicalize env (A.At typeRegion tipe) =
  case tipe of
    Src.TVar x ->
        Result.ok (Can.TVar x)

    Src.TType region name args ->
        canonicalizeType env typeRegion name args =<<
          Env.findType region env name

    Src.TTypeQual region home name args ->
        canonicalizeType env typeRegion name args =<<
          Env.findTypeQual region env home name

    Src.TLambda a b ->
        Can.TLambda
          <$> canonicalize env a
          <*> canonicalize env b

    Src.TRecord fields ext ->
        do  cfields <- sequenceA =<< Dups.checkFields (canonicalizeFields env fields)
            return $ Can.TRecord cfields (fmap A.toValue ext)

    Src.TUnit ->
        Result.ok Can.TUnit

    Src.TTuple a b cs ->
        Can.TTuple
          <$> canonicalize env a
          <*> canonicalize env b
          <*>
            case cs of
              [] ->
                Result.ok Nothing

              [c] ->
                Just <$> canonicalize env c

              _ ->
                Result.throw $ Error.TupleLargerThanThree typeRegion

    Src.TTagRow entries ext ->
        do  centries <- traverse (canonicalizeTagEntry env) entries
            tagMap <- foldM insertTagEntry Map.empty centries
            Result.ok $ Can.TTagRow tagMap (fmap A.toValue ext)


canonicalizeTagEntry :: Env.Env -> Src.TagEntry -> Result i w (A.Region, Can.TagKey, [Can.Type])
canonicalizeTagEntry env (Src.TagEntry region maybeHome name args) =
  do  ctor <-
        case maybeHome of
          Nothing ->
            Env.findCtor region env name

          Just home ->
            Env.findCtorQual region env home name

      case ctor of
        Env.TagCtor tagHome params ->
          do  cargs <- traverse (canonicalize env) args
              checkArity (length params) region name args
                (region, (tagHome, name), cargs)

        Env.Ctor _ _ _ _ _ ->
          Result.throw (Error.TagRowNotATag region name)

        Env.RecordCtor _ _ _ ->
          Result.throw (Error.TagRowNotATag region name)


insertTagEntry
  :: Map.Map Can.TagKey [Can.Type]
  -> (A.Region, Can.TagKey, [Can.Type])
  -> Result i w (Map.Map Can.TagKey [Can.Type])
insertTagEntry tagMap (region, key@(_, name), args) =
  if Map.member key tagMap then
    Result.throw (Error.TagRowDuplicate region name)
  else
    Result.ok (Map.insert key args tagMap)


canonicalizeFields :: Env.Env -> [(A.Located Name.Name, Src.Type)] -> [(A.Located Name.Name, Result i w Can.FieldType)]
canonicalizeFields env fields =
  let
    len = fromIntegral (length fields)
    canonicalizeField index (name, srcType) =
      (name, Can.FieldType index <$> canonicalize env srcType)
  in
  zipWith canonicalizeField [0..len] fields



-- CANONICALIZE TYPE


canonicalizeType :: Env.Env -> A.Region -> Name.Name -> [Src.Type] -> Env.Type -> Result i w Can.Type
canonicalizeType env region name args info =
  do  cargs <- traverse (canonicalize env) args
      case info of
        Env.Alias arity home argNames aliasedType ->
          checkArity arity region name args $
            Can.TAlias home name (zip argNames cargs) (Can.Holey aliasedType)

        Env.Union arity home ->
          checkArity arity region name args $
            Can.TType home name cargs


checkArity :: Int -> A.Region -> Name.Name -> [A.Located arg] -> answer -> Result i w answer
checkArity expected region name args answer =
  let actual = length args in
  if expected == actual then
    Result.ok answer
  else
    Result.throw (Error.BadArity region Error.TypeArity name expected actual)



-- ADD FREE VARS


addFreeVars :: Map.Map Name.Name () -> Can.Type -> Map.Map Name.Name ()
addFreeVars freeVars tipe =
  case tipe of
    Can.TLambda arg result ->
      addFreeVars (addFreeVars freeVars result) arg

    Can.TVar var ->
      Map.insert var () freeVars

    Can.TType _ _ args ->
      List.foldl' addFreeVars freeVars args

    Can.TRecord fields Nothing ->
      Map.foldl addFieldFreeVars freeVars fields

    Can.TRecord fields (Just ext) ->
      Map.foldl addFieldFreeVars (Map.insert ext () freeVars) fields

    Can.TUnit ->
      freeVars

    Can.TTuple a b maybeC ->
      case maybeC of
        Nothing ->
          addFreeVars (addFreeVars freeVars a) b

        Just c ->
          addFreeVars (addFreeVars (addFreeVars freeVars a) b) c

    Can.TAlias _ _ args _ ->
      List.foldl' (\fvs (_,arg) -> addFreeVars fvs arg) freeVars args

    Can.TTagRow tags Nothing ->
      Map.foldl (List.foldl' addFreeVars) freeVars tags

    Can.TTagRow tags (Just ext) ->
      Map.foldl (List.foldl' addFreeVars) (Map.insert ext () freeVars) tags


addFieldFreeVars :: Map.Map Name.Name () -> Can.FieldType -> Map.Map Name.Name ()
addFieldFreeVars freeVars (Can.FieldType _ tipe) =
  addFreeVars freeVars tipe
