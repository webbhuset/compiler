module Type.Overload
  ( Need(..)
  , record
  , resolveModule
  , Target(..)
  , lookupResolved
  )
  where


import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Map as Map
import qualified Data.Name as Name
import System.IO.Unsafe (unsafePerformIO)

import qualified AST.Canonical as Can
import qualified AST.Utils.Type as Type
import qualified Canonicalize.Overload as Overload
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Overload as Error
import Type.Type (Variable)
import qualified Type.Type as Solver



-- RESOLVING OVERLOADED USE SITES
--
-- Which definition an overloaded name means depends on the type it is used
-- at, and that type is only known once the solver has run. Elm keeps no typed
-- AST, so a use site is recorded during constraint generation together with
-- the variables standing for the types it dispatches on, and read back
-- afterwards.
--
-- The tables are global because the type checker has no environment to thread
-- this through, exactly as Type.Comparable does. Keys carry the module, so
-- modules compiled in parallel cannot collide, and both tables only ever grow.


-- One overload a use site needs, and the variable whose solved type says
-- which definition that is.
data Need =
  Need Can.OverloadName Variable


data Site =
  Site
    { _needs :: [Need]
    -- The `where` clauses of the definition this site is in, paired with the
    -- rigid variable each one dispatches on. A site typed at one of those
    -- variables takes the clause's parameter instead of a definition.
    , _clauses :: [(Can.Constraint, Variable)]
    }


-- What a use site turned out to mean.
data Target
  = Definition Can.OverloadName [Target]  -- a definition, and what it needs in turn
  | Parameter Name.Name                   -- a `where` clause of the enclosing definition


{-# NOINLINE sitesRef #-}
sitesRef :: IORef (Map.Map ModuleName.Canonical [(A.Region, Site)])
sitesRef =
  unsafePerformIO (newIORef Map.empty)


{-# NOINLINE resolvedRef #-}
resolvedRef :: IORef (Map.Map (ModuleName.Canonical, A.Region) [Target])
resolvedRef =
  unsafePerformIO (newIORef Map.empty)


record :: ModuleName.Canonical -> A.Region -> [Need] -> [(Can.Constraint, Variable)] -> IO ()
record home region needs clauses =
  atomicModifyIORef' sitesRef $ \table ->
    ( Map.insertWith (++) home [(region, Site needs clauses)] table, () )



-- RESOLVE
--
-- Called once the solver has finished, so every recorded variable now points
-- at the type its use site was used at. Returns the sites it could not settle,
-- for the caller to report.


resolveModule :: ModuleName.Canonical -> Can.Overloads -> IO [Error.Error]
resolveModule home overloads =
  do  table <- readIORef sitesRef
      let sites = Map.findWithDefault [] home table
      concat <$> traverse (resolveSite home overloads) sites


resolveSite :: ModuleName.Canonical -> Can.Overloads -> (A.Region, Site) -> IO [Error.Error]
resolveSite home overloads (region, Site needs clauses) =
  do  -- Rendered together so that two of these come out written with the same
      -- type variable name only when they really are the same variable.
      types <-
        Solver.toRelatedTypes $
          map (\(Need _ var) -> var) needs ++ map snd clauses

      let (needTypes, clauseTypes) = splitAt (length needs) types
      let inScope =
            [ (constraint, var)
            | ((constraint, _), Can.TVar var) <- zip clauses clauseTypes
            ]

      case traverse (uncurry (resolve overloads inScope)) (zip needs needTypes) of
        Left toError ->
          return [toError region]

        Right targets ->
          do  atomicModifyIORef' resolvedRef $ \resolved ->
                ( Map.insert (home, region) targets resolved, () )
              return []


resolve
  :: Can.Overloads
  -> [(Can.Constraint, Name.Name)]
  -> Need
  -> Can.Type
  -> Either (A.Region -> Error.Error) Target
resolve overloads inScope (Need ovName _) tipe =
  resolveAt overloads inScope ovName (Just tipe)


resolveAt
  :: Can.Overloads
  -> [(Can.Constraint, Name.Name)]
  -> Can.OverloadName
  -> Maybe Can.Type
  -> Either (A.Region -> Error.Error) Target
resolveAt overloads inScope ovName@(ovHome, name) rawDispatched =
  case Type.iteratedDealias <$> rawDispatched of
    Nothing ->
      Left $ \region -> Error.Ambiguous region ovHome name Nothing

    Just (Can.TVar var) ->
      case [ c | (c@(Can.Constraint n _), v) <- inScope, n == ovName, v == var ] of
        constraint : _ ->
          Right (Parameter (Overload.dictName constraint))

        [] ->
          -- `number` and friends are not type variables anyone can name in a
          -- clause, they are types the checker has not pinned down yet.
          if isSuper var then
            Left $ \region -> Error.Ambiguous region ovHome name (Just (Can.TVar var))
          else
            Left $ \region ->
              Error.NoClause region ovHome name var $
                Overload.substitute var <$> Map.lookup ovName (Can._abstracts overloads)

    Just dispatched ->
      case dispatchKey dispatched of
        Nothing ->
          Left $ \region -> Error.Ambiguous region ovHome name (Just dispatched)

        Just key ->
          case Map.lookup key =<< Map.lookup ovName (Can._instances overloads) of
            Nothing ->
              Left $ \region -> Error.NoDefinition region ovHome name dispatched

            Just (Can.Instance target declared) ->
              -- The definition may itself need overloads, as one for `List a`
              -- does. Matching what it was declared for against what it is
              -- being used at says which types those are needed at.
              case Map.lookup target (Can._constrained overloads) of
                Nothing ->
                  Right (Definition target [])

                Just clauses ->
                  let subst = match declared dispatched Map.empty in
                  Definition target <$>
                    traverse
                      (\c@(Can.Constraint n _) ->
                        resolveAt overloads inScope n
                          (Map.lookup (Overload.dispatchVar c) subst))
                      clauses



isSuper :: Name.Name -> Bool
isSuper name =
  Name.isNumberType name || Name.isComparableType name
  || Name.isAppendableType name || Name.isCompappendType name



-- MATCHING
--
-- What a definition was declared for against what it is being used at, so
-- that `List a` matched with `List Card` says `a` is `Card`.


match :: Can.Type -> Can.Type -> Map.Map Name.Name Can.Type -> Map.Map Name.Name Can.Type
match declared actual subst =
  case (declared, actual) of
    (Can.TAlias _ _ _ (Can.Holey t), _) ->
      match t actual subst

    (Can.TAlias _ _ _ (Can.Filled t), _) ->
      match t actual subst

    (_, Can.TAlias _ _ _ (Can.Holey t)) ->
      match declared t subst

    (_, Can.TAlias _ _ _ (Can.Filled t)) ->
      match declared t subst

    (Can.TVar v, _) ->
      Map.insert v actual subst

    (Can.TType _ _ as, Can.TType _ _ bs) ->
      foldr (uncurry match) subst (zip as bs)

    (Can.TLambda a1 b1, Can.TLambda a2 b2) ->
      match a1 a2 (match b1 b2 subst)

    (Can.TTagRow tags1 _, Can.TTagRow tags2 _) ->
      foldr (uncurry match) subst $
        concat (Map.elems (Map.intersectionWith zip tags1 tags2))

    (Can.TTuple a1 b1 c1, Can.TTuple a2 b2 c2) ->
      match a1 a2 $ match b1 b2 $
        case (c1, c2) of
          (Just x, Just y) -> match x y subst
          _                -> subst

    _ ->
      subst



-- DISPATCH


-- A definition is chosen by the head constructor of the dispatch type, so a
-- record has nothing to dispatch on.
dispatchKey :: Can.Type -> Maybe Can.OverloadKey
dispatchKey =
  Overload.dispatchKey


-- LOOK UP
--
-- Used while optimizing, once the module's sites have all been resolved.


{-# NOINLINE lookupResolved #-}
lookupResolved :: ModuleName.Canonical -> A.Region -> [Target]
lookupResolved home region =
  unsafePerformIO (Map.findWithDefault [] (home, region) <$> readIORef resolvedRef)
