{-# LANGUAGE BangPatterns #-}
module Type.Overload
  ( Site(..)
  , record
  , resolveModule
  , lookupResolved
  )
  where


import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Overload as Error
import Type.Type (Variable)
import qualified Type.Type as Type



-- RESOLVING OVERLOADED USE SITES
--
-- Which definition `Ord.compare` means depends on the type it is used at,
-- and that type is only known once the solver has run. Elm keeps no typed
-- AST, so the use site is recorded during constraint generation together
-- with the variable standing for its type, and read back afterwards.
--
-- The tables are global because the type checker has no environment to
-- thread this through, exactly as Type.Comparable does. Keys carry the
-- module, so modules compiled in parallel cannot collide, and both tables
-- only ever grow.


data Site =
  Site
    { _site_name :: Can.OverloadName
    , _site_var :: Variable
    }


{-# NOINLINE sitesRef #-}
sitesRef :: IORef (Map.Map ModuleName.Canonical [(A.Region, Site)])
sitesRef =
  unsafePerformIO (newIORef Map.empty)


{-# NOINLINE resolvedRef #-}
resolvedRef :: IORef (Map.Map (ModuleName.Canonical, A.Region) Can.OverloadName)
resolvedRef =
  unsafePerformIO (newIORef Map.empty)


record :: ModuleName.Canonical -> A.Region -> Can.OverloadName -> Variable -> IO ()
record home region name var =
  atomicModifyIORef' sitesRef $ \table ->
    ( Map.insertWith (++) home [(region, Site name var)] table, () )



-- RESOLVE
--
-- Called once the solver has finished, so every recorded variable now
-- points at the type its use site was used at. Returns the sites it could
-- not settle, for the caller to report.


resolveModule :: ModuleName.Canonical -> Can.Overloads -> IO [Error.Error]
resolveModule home (Can.Overloads _ instances) =
  do  table <- readIORef sitesRef
      let sites = Map.findWithDefault [] home table
      concat <$> traverse (resolveSite home instances) sites


resolveSite
  :: ModuleName.Canonical
  -> Map.Map Can.OverloadName (Map.Map Can.OverloadKey Can.OverloadName)
  -> (A.Region, Site)
  -> IO [Error.Error]
resolveSite home instances (region, Site (ovHome, ovName) var) =
  do  Can.Forall _ tipe <- Type.toAnnotation var
      case dispatchType tipe of
        Nothing ->
          return [Error.Ambiguous region ovHome ovName tipe]

        Just dispatched ->
          case dispatchKey dispatched of
            Nothing ->
              return [Error.Ambiguous region ovHome ovName dispatched]

            Just key ->
              case Map.lookup key =<< Map.lookup (ovHome, ovName) instances of
                Nothing ->
                  return [Error.NoDefinition region ovHome ovName dispatched tipe]

                Just target ->
                  do  atomicModifyIORef' resolvedRef $ \resolved ->
                        ( Map.insert (home, region) target resolved, () )
                      return []


-- Dispatch is on the first argument, which is why an abstract signature
-- has to be a function whose first argument is a type variable.
dispatchType :: Can.Type -> Maybe Can.Type
dispatchType tipe =
  case tipe of
    Can.TLambda arg _ -> Just arg
    Can.TAlias _ _ _ (Can.Holey t) -> dispatchType t
    Can.TAlias _ _ _ (Can.Filled t) -> dispatchType t
    _ -> Nothing


-- A definition is chosen by the head constructor of the dispatch type, so
-- a type variable, a record or a tuple has nothing to dispatch on.
dispatchKey :: Can.Type -> Maybe Can.OverloadKey
dispatchKey tipe =
  case tipe of
    Can.TType home name _ -> Just (home, name)
    Can.TAlias home name _ _ -> Just (home, name)
    _ -> Nothing



-- LOOK UP
--
-- Used while optimizing, once the module's sites have all been resolved.


{-# NOINLINE lookupResolved #-}
lookupResolved :: ModuleName.Canonical -> A.Region -> Maybe Can.OverloadName
lookupResolved home region =
  unsafePerformIO (Map.lookup (home, region) <$> readIORef resolvedRef)
