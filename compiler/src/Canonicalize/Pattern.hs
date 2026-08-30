module Canonicalize.Pattern
  ( verify
  , Bindings
  , DupsDict
  , canonicalize
  )
  where


import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Dups as Dups
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result



-- RESULTS


type Result i w a =
  Result.Result i w Error.Error a


type Bindings =
  Map.Map Name.Name A.Region



-- VERIFY


verify :: Error.DuplicatePatternContext -> Result DupsDict w a -> Result i w (a, Bindings)
verify context (Result.Result k) =
  Result.Result $ \info warnings bad good ->
    k Dups.none warnings
      (\_ warnings1 errors ->
          bad info warnings1 errors
      )
      (\bindings warnings1 value ->
          case Dups.detect (Error.DuplicatePattern context) bindings of
            Result.Result k1 ->
              k1 () ()
                (\() () errs -> bad info warnings1 errs)
                (\() () dict -> good info warnings1 (value, dict))
      )



-- CANONICALIZE


type DupsDict =
  Dups.Dict A.Region


canonicalize :: Env.Env -> Src.Pattern -> Result DupsDict w Can.Pattern
canonicalize env pattern =
  canonicalizeHelp env True pattern


-- The Bool tracks whether a structural variant tag pattern is allowed at this
-- position. Tags may appear at the top of a pattern, as arguments to other
-- tag patterns, and as a constructor argument whose declared type is a type
-- VARIABLE, like the `e` of `Err : e -> Result e a`.
--
-- Everywhere else they are rejected, because closing the variant row is what
-- makes a tag match exhaustive and the closing can only be computed for
-- those positions. In a constructor argument of some fixed type, or inside a
-- tuple or a list, an unhandled tag would slip through and crash at runtime.
--
canonicalizeHelp :: Env.Env -> Bool -> Src.Pattern -> Result DupsDict w Can.Pattern
canonicalizeHelp env tagAllowed (A.At region pattern) =
  A.At region <$>
  case pattern of
    Src.PAnything ->
      Result.ok Can.PAnything

    Src.PVar name ->
      logVar name region (Can.PVar name)

    Src.PRecord fields ->
      logFields fields (Can.PRecord (map A.toValue fields))

    Src.PUnit ->
      Result.ok Can.PUnit

    Src.PTuple a b cs ->
      Can.PTuple
        <$> canonicalizeHelp env False a
        <*> canonicalizeHelp env False b
        <*> canonicalizeTuple region env cs

    Src.PCtor nameRegion name patterns ->
      canonicalizeCtor env tagAllowed region name patterns =<< Env.findCtor nameRegion env name

    Src.PCtorQual nameRegion home name patterns ->
      canonicalizeCtor env tagAllowed region name patterns =<< Env.findCtorQual nameRegion env home name

    Src.PList patterns ->
      Can.PList <$> canonicalizeList env patterns

    Src.PCons first rest ->
      Can.PCons
        <$> canonicalizeHelp env False first
        <*> canonicalizeHelp env False rest

    Src.PAlias ptrn (A.At reg name) ->
      do  cpattern <- canonicalizeHelp env tagAllowed ptrn
          logVar name reg (Can.PAlias cpattern name)

    Src.PChr chr ->
      Result.ok (Can.PChr chr)

    Src.PStr str ->
      Result.ok (Can.PStr str)

    Src.PInt int ->
      Result.ok (Can.PInt (fromIntegral int)) -- TODO make overflow an error


canonicalizeCtor :: Env.Env -> Bool -> A.Region -> Name.Name -> [Src.Pattern] -> Env.Ctor -> Result DupsDict w Can.Pattern_
canonicalizeCtor env tagAllowed region name patterns ctor =
  case ctor of
    Env.Ctor home tipe union index args ->
      let
        toCanonicalArg argIndex argPattern argTipe =
          Can.PatternCtorArg argIndex argTipe
            <$> canonicalizeHelp env (isTypeVar argTipe) argPattern
      in
      do  verifiedList <- Index.indexedZipWithA toCanonicalArg patterns args
          case verifiedList of
            Index.LengthMatch cargs ->
              if tipe == Name.bool && home == ModuleName.basics then
                Result.ok (Can.PBool union (name == Name.true))
              else
                Result.ok (Can.PCtor home tipe union name index cargs)

            Index.LengthMismatch actualLength expectedLength ->
              Result.throw (Error.BadArity region Error.PatternArity name expectedLength actualLength)

    Env.TagCtor home params ->
      if not tagAllowed then
        Result.throw (Error.TagPatternNesting region name)
      else if length patterns /= length params then
        Result.throw (Error.BadArity region Error.PatternArity name (length params) (length patterns))
      else
        do  cargs <- traverse (canonicalizeHelp env True) patterns
            Result.ok (Can.PTag home name params cargs)

    Env.RecordCtor _ _ _ ->
      Result.throw (Error.PatternHasRecordCtor region name)


-- A constructor argument declared as a bare type variable can hold a variant
-- row, so a tag pattern there can be closed like any other column.
isTypeVar :: Can.Type -> Bool
isTypeVar tipe =
  case tipe of
    Can.TVar _ -> True
    _ -> False


canonicalizeTuple :: A.Region -> Env.Env -> [Src.Pattern] -> Result DupsDict w (Maybe Can.Pattern)
canonicalizeTuple tupleRegion env extras =
  case extras of
    [] ->
      Result.ok Nothing

    [three] ->
      Just <$> canonicalizeHelp env False three

    _ ->
      Result.throw $ Error.TupleLargerThanThree tupleRegion


canonicalizeList :: Env.Env -> [Src.Pattern] -> Result DupsDict w [Can.Pattern]
canonicalizeList env list =
  case list of
    [] ->
      Result.ok []

    pattern : otherPatterns ->
      (:)
        <$> canonicalizeHelp env False pattern
        <*> canonicalizeList env otherPatterns



-- LOG BINDINGS


logVar :: Name.Name -> A.Region -> a -> Result DupsDict w a
logVar name region value =
  Result.Result $ \bindings warnings _ ok ->
    ok (Dups.insert name region region bindings) warnings value


logFields :: [A.Located Name.Name] -> a -> Result DupsDict w a
logFields fields value =
  let
    addField dict (A.At region name) =
      Dups.insert name region region dict
  in
  Result.Result $ \bindings warnings _ ok ->
    ok (List.foldl' addField bindings fields) warnings value
