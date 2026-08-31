module Type.Comparable
  ( Atom
  , Info(..)
  , compute
  , register
  , isComparableAtom
  )
  where


import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

import qualified AST.Canonical as Can
import qualified AST.Utils.Type as Type
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName



-- COMPARABLE NEWTYPES
--
-- A custom type with a single constructor holding a single concrete
-- comparable payload, like (type Id = Id String), is itself comparable.
-- Such types compile to their unwrapped payload in --optimize mode, and
-- the dev-mode runtime knows how to unwrap them, so ordering is simply
-- the ordering of the payload.
--
-- Whether a type qualifies is decided here, once, when its defining
-- module is canonicalized. The result is stored in the module interface
-- (Elm.Interface._comparables) as the set of qualifying types visible
-- from that module, including everything inherited from its imports.
-- That closure property means any type mentioned in any reachable type
-- annotation is covered by the interfaces at hand, no matter how deep
-- the definition lives.
--
-- The unifier needs this information but deliberately has no environment,
-- so the per-module result is also registered in a process-global table
-- before type checking starts (the same approach as the trusted kernel
-- packages in Elm.Package). Recompiles overwrite their own entries, so
-- long-lived processes like the repl stay correct when a type changes.
--


type Atom =
  ( ModuleName.Canonical, Name.Name )


data Info =
  Info
    { _atoms :: Set.Set Atom      -- all comparable newtypes visible to this module
    , _locals :: Map.Map Atom Bool -- judgments for the locally defined unions
    }



-- COMPUTE


compute :: Map.Map ModuleName.Raw I.Interface -> Can.Module -> Info
compute ifaces (Can.Module home _ _ _ unions _ _ _ _ _) =
  let
    imported =
      Set.unions (map I._comparables (Map.elems ifaces))

    locals =
      Map.fromList
        [ ((home, name), isComparableUnion home imported unions (Set.singleton name) union)
        | (name, union) <- Map.toList unions
        ]

    atoms =
      Set.union imported (Map.keysSet (Map.filter id locals))
  in
  Info atoms locals


isComparableUnion
  :: ModuleName.Canonical
  -> Set.Set Atom
  -> Map.Map Name.Name Can.Union
  -> Set.Set Name.Name
  -> Can.Union
  -> Bool
isComparableUnion home imported unions seen union =
  case union of
    Can.Union [] [Can.Ctor _ _ 1 [payload]] 1 _ ->
      isComparableType home imported unions seen payload

    _ ->
      False


isComparableType
  :: ModuleName.Canonical
  -> Set.Set Atom
  -> Map.Map Name.Name Can.Union
  -> Set.Set Name.Name
  -> Can.Type
  -> Bool
isComparableType home imported unions seen tipe =
  let
    go = isComparableType home imported unions seen
  in
  case tipe of
    Can.TAlias _ _ args aliased ->
      go (Type.dealias args aliased)

    Can.TType tipeHome name tipeArgs
      | tipeHome == ModuleName.basics && null tipeArgs && (name == Name.int || name == Name.float) ->
          True

      | tipeHome == ModuleName.string && null tipeArgs && name == Name.string ->
          True

      | tipeHome == ModuleName.char && null tipeArgs && name == Name.char ->
          True

      | tipeHome == ModuleName.list && name == Name.list ->
          case tipeArgs of
            [element] -> go element
            _         -> False

      | null tipeArgs ->
          if tipeHome == home
            then
              -- a locally defined type; recurse, guarding against cycles
              -- like (type A = A B; type B = B A)
              not (Set.member name seen)
              && (case Map.lookup name unions of
                    Just union -> isComparableUnion home imported unions (Set.insert name seen) union
                    Nothing    -> False)
            else
              Set.member (tipeHome, name) imported

      | otherwise ->
          False

    Can.TTuple a b maybeC ->
      go a && go b && maybe True go maybeC

    Can.TUnit ->
      False

    Can.TVar _ ->
      False

    Can.TLambda _ _ ->
      False

    Can.TRecord _ _ ->
      False

    Can.TTagRow _ _ ->
      False



-- GLOBAL REGISTRY


{-# NOINLINE registryRef #-}
registryRef :: IORef (Map.Map Atom Bool)
registryRef =
  unsafePerformIO (newIORef Map.empty)


register :: Info -> IO ()
register (Info atoms locals) =
  atomicModifyIORef' registryRef $ \table ->
    ( Map.union locals (Set.foldr (\atom -> Map.insert atom True) table atoms)
    , ()
    )


{-# NOINLINE isComparableAtom #-}
isComparableAtom :: ModuleName.Canonical -> Name.Name -> Bool
isComparableAtom home name =
  unsafePerformIO $
    Map.findWithDefault False (home, name) <$> readIORef registryRef
