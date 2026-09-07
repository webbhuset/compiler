{-# LANGUAGE OverloadedStrings #-}
module Type.Portable
  ( Atom
  , Info(..)
  , Problem(..)
  , checkPortable
  )
  where


import Data.Foldable (asum)
import Data.Maybe (maybeToList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Utils.Type as Type
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg



-- PORTABLE TYPES
--
-- A type is "portable" if every value of it survives a structured clone
-- across the web-worker boundary: no functions, and no kernel/effect types
-- (Cmd, Sub, Task, Decoder, Worker, Channel, ...) that wrap functions or
-- otherwise resist cloning. The judgment is a fail-closed whitelist: a type
-- is portable only if it is a scalar, a structural container of portable
-- arguments, plain user data whose fields are portable, or a foreign type
-- its defining module judged portable. See docs/web-workers.md.


type Atom =
  ( ModuleName.Canonical, Name.Name )


newtype Info =
  Info
    { _nonportables :: Set.Set Atom
      -- non-portable atoms visible from a module: the union of all its
      -- direct imports' sets plus its own non-portable local unions. Absence
      -- of an atom means it was judged portable by its defining module.
    }


-- why a type was rejected; drives the Nitpick.Workers error report
data Problem
  = PFunction            -- a function (TLambda) somewhere in the type
  | PBadAtom Atom        -- a non-portable named type (incl. kernel-module types)
  | PTypeVar Name.Name   -- a free type variable at the boundary
  | PExtensibleRecord    -- an open record / tag-row tail
  deriving (Eq)



-- CHECK
--
-- `Nothing` means portable. The two knobs threaded through the walk:
--
--   * `assumed` — a union's own type parameters, treated as portable while
--     its body is judged. Every use site independently checks the actual
--     arguments, so verdicts are "portable given portable arguments":
--     `Box Int` is fine and `Box (Int -> Int)` is not, from one verdict.
--
--   * `seen` — local unions currently being unfolded. Portability is a
--     greatest fixpoint, so revisiting a union means portable (a value like
--     `type Tree = Node (List Tree)` clones fine, cycles included).


checkPortable
  :: Info
  -> ModuleName.Canonical            -- home module (to unfold local unions)
  -> Map.Map Name.Name Can.Union     -- the home module's unions
  -> Can.Type
  -> Maybe Problem
checkPortable info home unions =
  go Set.empty Set.empty
  where
    nonportables =
      _nonportables info

    go assumed seen tipe =
      case tipe of
        Can.TLambda _ _ ->
          Just PFunction

        Can.TVar name ->
          if Set.member name assumed then Nothing else Just (PTypeVar name)

        Can.TUnit ->
          Nothing

        Can.TTuple a b maybeC ->
          asum (go assumed seen a : go assumed seen b : map (go assumed seen) (maybeToList maybeC))

        Can.TAlias _ _ args aliased ->
          go assumed seen (Type.dealias args aliased)

        Can.TRecord fields ext ->
          row assumed seen ext [ ft | Can.FieldType _ ft <- Map.elems fields ]

        Can.TTagRow tags ext ->
          row assumed seen ext (concat (Map.elems tags))

        Can.TType tipeHome name args ->
          goType assumed seen tipeHome name args

    row assumed seen ext payloads =
      case ext of
        Just _  -> Just PExtensibleRecord
        Nothing -> asum (map (go assumed seen) payloads)

    goType assumed seen tipeHome name args
      | isScalar tipeHome name =
          Nothing

      | isContainer tipeHome name =
          asum (map (go assumed seen) args)

      | tipeHome == home =
          -- a local union: judge its body parametrically, then check the
          -- actual arguments at this use site
          asum (localUnion seen name : map (go assumed seen) args)

      | Set.member (tipeHome, name) nonportables =
          Just (PBadAtom (tipeHome, name))

      | otherwise =
          -- a foreign type its own module judged portable-given-arguments
          asum (map (go assumed seen) args)

    localUnion seen name
      | Set.member name seen =
          Nothing

      | otherwise =
          case Map.lookup name unions of
            Nothing ->
              Just (PBadAtom (home, name))

            Just (Can.Union vars ctors _ _) ->
              asum
                [ go (Set.fromList vars) (Set.insert name seen) payload
                | Can.Ctor _ _ _ payloads <- ctors
                , payload <- payloads
                ]



-- WHITELISTED HEADS
--
-- Scalars and structural containers are portable regardless of the stored
-- verdict for their home module (List/Array/Dict live in kernel-importing
-- core modules, so their own verdict is non-portable; these cases shadow it).


isScalar :: ModuleName.Canonical -> Name.Name -> Bool
isScalar home name =
  (home == ModuleName.basics && (name == Name.int || name == Name.float || name == "Bool"))
  || (home == ModuleName.string && name == Name.string)
  || (home == ModuleName.char && name == Name.char)


isContainer :: ModuleName.Canonical -> Name.Name -> Bool
isContainer home name =
  (home == ModuleName.list && name == Name.list)
  || (home == ModuleName.array && name == Name.array)
  || (home == ModuleName.dict && name == Name.dict)
  || (home == setModule && name == "Set")
  || (home == ModuleName.maybe && name == Name.maybe)
  || (home == ModuleName.result && name == Name.result)


setModule :: ModuleName.Canonical
setModule =
  ModuleName.Canonical Pkg.core "Set"
