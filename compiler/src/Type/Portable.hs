{-# LANGUAGE OverloadedStrings #-}
module Type.Portable
  ( Atom
  , Info(..)
  , Problem(..)
  , checkPortable
  , computeLocals
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
-- arguments, plain user data whose fields are portable, or a named type its
-- defining module judged portable. See docs/web-workers.md.


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



-- USE-SITE CHECK
--
-- `Nothing` means portable. Two knobs are threaded through the walk:
--
--   * `assumed` — a union's own type parameters, treated as portable while
--     its body is judged. Every use site independently checks the actual
--     arguments, so verdicts are "portable given portable arguments":
--     `Box Int` is fine and `Box (Int -> Int)` is not, from one verdict.
--
--   * `seen` — local unions currently being unfolded. Portability is a
--     greatest fixpoint, so revisiting a union means portable (a value like
--     `type Tree = Node (List Tree)` clones fine, cycles included).


data Env =
  Env
    { _envNonportables :: Set.Set Atom
    , _envHome :: ModuleName.Canonical
    , _envUnions :: Map.Map Name.Name Can.Union
    }


checkPortable
  :: Info
  -> ModuleName.Canonical            -- home module (to unfold local unions)
  -> Map.Map Name.Name Can.Union     -- the home module's unions
  -> Can.Type
  -> Maybe Problem
checkPortable (Info nonportables) home unions =
  go (Env nonportables home unions) Set.empty Set.empty


go :: Env -> Set.Set Name.Name -> Set.Set Name.Name -> Can.Type -> Maybe Problem
go env assumed seen tipe =
  case tipe of
    Can.TLambda _ _ ->
      Just PFunction

    Can.TVar name ->
      if Set.member name assumed then Nothing else Just (PTypeVar name)

    Can.TUnit ->
      Nothing

    Can.TTuple a b maybeC ->
      asum (go env assumed seen a : go env assumed seen b : map (go env assumed seen) (maybeToList maybeC))

    Can.TAlias _ _ args aliased ->
      go env assumed seen (Type.dealias args aliased)

    Can.TRecord fields ext ->
      row env assumed seen ext [ ft | Can.FieldType _ ft <- Map.elems fields ]

    Can.TTagRow tags ext ->
      row env assumed seen ext (concat (Map.elems tags))

    Can.TType tipeHome name args ->
      goType env assumed seen tipeHome name args


row :: Env -> Set.Set Name.Name -> Set.Set Name.Name -> Maybe Name.Name -> [Can.Type] -> Maybe Problem
row env assumed seen ext payloads =
  case ext of
    Just _  -> Just PExtensibleRecord
    Nothing -> asum (map (go env assumed seen) payloads)


goType :: Env -> Set.Set Name.Name -> Set.Set Name.Name -> ModuleName.Canonical -> Name.Name -> [Can.Type] -> Maybe Problem
goType env assumed seen tipeHome name args
  | isScalar tipeHome name =
      Nothing

  | isContainer tipeHome name =
      asum (map (go env assumed seen) args)

  | tipeHome == _envHome env =
      -- a local union: judge its body parametrically, then check the actual
      -- arguments at this use site
      asum (localUnion env seen name : map (go env assumed seen) args)

  | Set.member (tipeHome, name) (_envNonportables env) =
      Just (PBadAtom (tipeHome, name))

  | otherwise =
      -- a foreign type its own module judged portable-given-arguments
      asum (map (go env assumed seen) args)


localUnion :: Env -> Set.Set Name.Name -> Name.Name -> Maybe Problem
localUnion env seen name
  | Set.member name seen =
      Nothing

  | otherwise =
      case Map.lookup name (_envUnions env) of
        Nothing ->
          Just (PBadAtom (_envHome env, name))

        Just union ->
          judgeUnion env seen name union


judgeUnion :: Env -> Set.Set Name.Name -> Name.Name -> Can.Union -> Maybe Problem
judgeUnion env seen name (Can.Union vars ctors _ _) =
  asum
    [ go env (Set.fromList vars) (Set.insert name seen) payload
    | Can.Ctor _ _ _ payloads <- ctors
    , payload <- payloads
    ]



-- DEFINITION-SITE VERDICTS
--
-- When a module is compiled it judges each of its own unions and produces
-- the set of non-portable atoms visible from it: the verdicts inherited
-- from its imports, plus its own local unions that are non-portable. That
-- closure means a later module reading this one's interface never has to
-- unfold across module boundaries -- every reachable atom already carries a
-- verdict (the same trick as Type.Comparable).
--
-- The kernel rule is the fail-closed core: a union defined in a module that
-- imports kernel code is non-portable unless it is on the allow-list, so a
-- phantom kernel type (Cmd, Worker, a user git-dep kernel type) that would
-- structurally look portable is still rejected.


computeLocals :: Set.Set Atom -> Bool -> ModuleName.Canonical -> Map.Map Name.Name Can.Union -> Info
computeLocals imported usesKernel home unions =
  let
    env =
      Env imported home unions

    localBad =
      Set.fromList
        [ (home, name)
        | (name, union) <- Map.toList unions
        , isLocalNonPortable usesKernel home name (judgeUnion env Set.empty name union)
        ]
  in
  Info (Set.union imported localBad)


isLocalNonPortable :: Bool -> ModuleName.Canonical -> Name.Name -> Maybe Problem -> Bool
isLocalNonPortable usesKernel home name structuralVerdict
  | usesKernel && not (Set.member (home, name) allowList) =
      True

  | otherwise =
      case structuralVerdict of
        Nothing -> False
        Just _  -> True



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



-- KERNEL ALLOW-LIST
--
-- Plain-data types that live in kernel-importing modules and clone fine, so
-- the kernel rule must not reject them.


allowList :: Set.Set Atom
allowList =
  Set.fromList
    [ (ModuleName.basics, "Order")
    , (ModuleName.jsonEncode, "Value")
    , (ModuleName.Canonical Pkg.bytes "Bytes", "Bytes")
    , (timeModule, "Posix")
    , (timeModule, "Zone")
    ]


timeModule :: ModuleName.Canonical
timeModule =
  ModuleName.Canonical Pkg.time "Time"
