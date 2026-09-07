module Type.Comparable
  ( Atom
  , Positions
  , Info(..)
  , compute
  , register
  , comparablePositions
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
-- A custom type with a single constructor holding a single comparable
-- payload, like (type Id = Id String), is itself comparable. Such types
-- compile to their unwrapped payload in --optimize mode, and the dev-mode
-- runtime knows how to unwrap them, so ordering is simply the ordering of
-- the payload.
--
-- The type may have parameters. Comparability of (type Box a = Box a)
-- depends on `a`, exactly as it does for (List a), while a parameter the
-- payload never mentions, as in (type Id t = Id String), cannot matter.
-- So the judgment for a type is not a yes or no but the positions of the
-- type arguments that have to be comparable in turn: none for `Id`, the
-- first for `Box`. A type that does not qualify at all has no entry.
--
-- Whether a type qualifies is decided here, once, when its defining
-- module is canonicalized. The result is stored in the module interface
-- (Elm.Interface._comparables) for every qualifying type visible from
-- that module, including everything inherited from its imports. That
-- closure property means any type mentioned in any reachable type
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


-- Indices of the type arguments that must be comparable, in order.
type Positions =
  [Int]


data Info =
  Info
    { _atoms :: Map.Map Atom Positions          -- all comparable newtypes visible to this module
    , _locals :: Map.Map Atom (Maybe Positions) -- judgments for the locally defined unions
    }



-- COMPUTE


compute :: Map.Map ModuleName.Raw I.Interface -> Can.Module -> Info
compute ifaces (Can.Module home _ _ _ unions _ _ _ _ _) =
  let
    imported =
      Map.unions (map I._comparables (Map.elems ifaces))

    locals =
      Map.fromList
        [ ((home, name), judgeUnion home imported unions (Set.singleton name) union)
        | (name, union) <- Map.toList unions
        ]

    atoms =
      Map.union imported (Map.mapMaybe id locals)
  in
  Info atoms locals


judgeUnion
  :: ModuleName.Canonical
  -> Map.Map Atom Positions
  -> Map.Map Name.Name Can.Union
  -> Set.Set Name.Name
  -> Can.Union
  -> Maybe Positions
judgeUnion home imported unions seen union =
  case union of
    Can.Union vars [Can.Ctor _ _ 1 [payload]] 1 _ ->
      do  needed <- judgeType home imported unions seen payload
          Just [ i | (i, var) <- zip [0..] vars, Set.member var needed ]

    _ ->
      Nothing


-- Whether a payload type is comparable, and if so which of the enclosing
-- type's parameters it needs to be comparable for that to hold. Every type
-- variable in a payload is one of those parameters, since a type
-- declaration cannot mention variables it does not bind.
judgeType
  :: ModuleName.Canonical
  -> Map.Map Atom Positions
  -> Map.Map Name.Name Can.Union
  -> Set.Set Name.Name
  -> Can.Type
  -> Maybe (Set.Set Name.Name)
judgeType home imported unions seen tipe =
  let
    go = judgeType home imported unions seen

    goAll types =
      Set.unions <$> traverse go types

    -- the arguments at the given positions must be comparable
    at positions args =
      goAll [ arg | (i, arg) <- zip [0..] args, i `elem` positions ]
  in
  case tipe of
    Can.TAlias _ _ args aliased ->
      go (Type.dealias args aliased)

    Can.TType tipeHome name tipeArgs
      | tipeHome == ModuleName.basics && null tipeArgs && (name == Name.int || name == Name.float) ->
          Just Set.empty

      | tipeHome == ModuleName.string && null tipeArgs && name == Name.string ->
          Just Set.empty

      | tipeHome == ModuleName.char && null tipeArgs && name == Name.char ->
          Just Set.empty

      | tipeHome == ModuleName.list && name == Name.list ->
          case tipeArgs of
            [element] -> go element
            _         -> Nothing

      | tipeHome == home ->
          -- a locally defined type; recurse, guarding against cycles
          -- like (type A = A B; type B = B A)
          if Set.member name seen then
            Nothing
          else
            do  union <- Map.lookup name unions
                positions <- judgeUnion home imported unions (Set.insert name seen) union
                at positions tipeArgs

      | otherwise ->
          do  positions <- Map.lookup (tipeHome, name) imported
              at positions tipeArgs

    Can.TTuple a b maybeC ->
      goAll (a : b : maybe [] pure maybeC)

    Can.TVar var ->
      Just (Set.singleton var)

    Can.TUnit ->
      Nothing

    Can.TLambda _ _ ->
      Nothing

    Can.TRecord _ _ ->
      Nothing

    Can.TTagRow _ _ ->
      Nothing



-- GLOBAL REGISTRY


{-# NOINLINE registryRef #-}
registryRef :: IORef (Map.Map Atom (Maybe Positions))
registryRef =
  unsafePerformIO (newIORef Map.empty)


register :: Info -> IO ()
register (Info atoms locals) =
  atomicModifyIORef' registryRef $ \table ->
    ( Map.union locals (Map.union (Map.map Just atoms) table)
    , ()
    )


-- The argument positions that must be comparable for this type to be, or
-- Nothing when the type is not a comparable newtype at all.
{-# NOINLINE comparablePositions #-}
comparablePositions :: ModuleName.Canonical -> Name.Name -> Maybe Positions
comparablePositions home name =
  unsafePerformIO $
    Map.findWithDefault Nothing (home, name) <$> readIORef registryRef
