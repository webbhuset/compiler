{-# LANGUAGE PatternGuards #-}
module Tool.Unify
  ( fits
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map

import qualified String as S

import qualified AST.Canonical as Can
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified AST.Utils.Type as Type



-- FITS
--
-- How many more arguments a value of the first type needs to become the
-- second, if any count up to its arity does. Both types may have variables,
-- and a variable in the expected type can be filled in, since the place
-- being filled may be polymorphic itself. A value whose result is a bare
-- variable, like `identity` or `Debug.todo`, fits anywhere and is left out.


fits :: Can.Type -> Can.Type -> Maybe Int
fits candidate expected =
  let
    c = rename "1" (Type.deepDealias candidate)
    e = rename "2" (Type.deepDealias expected)
    results = zip [0..] (resultsOf c)
    try (k, r) =
      case r of
        Can.TVar _ -> Nothing
        _          -> k <$ unify Map.empty r e
  in
  case [ k | Just k <- map try results ] of
    k : _ -> Just k
    []    -> Nothing


-- The type, then what is left after each argument.
resultsOf :: Can.Type -> [Can.Type]
resultsOf t =
  case t of
    Can.TLambda _ r -> t : resultsOf r
    _               -> [t]



-- UNIFY


type Subst =
  Map.Map String Can.Type


unify :: Subst -> Can.Type -> Can.Type -> Maybe Subst
unify subst a b =
  case (walk subst a, walk subst b) of
    (Can.TVar x, Can.TVar y) | T.varToChars x == T.varToChars y ->
      Just subst

    (Can.TVar x, t) -> bind subst x t
    (t, Can.TVar y) -> bind subst y t

    (Can.TLambda a1 r1, Can.TLambda a2 r2) ->
      unify subst a1 a2 >>= \s -> unify s r1 r2

    (Can.TType h1 n1 as1, Can.TType h2 n2 as2)
      | h1 == h2 && n1 == n2 && length as1 == length as2 ->
          unifyAll subst (zip as1 as2)

    (Can.TUnit, Can.TUnit) ->
      Just subst

    (Can.TPair a1 b1, Can.TPair a2 b2) ->
      unifyAll subst [(a1, a2), (b1, b2)]

    (Can.TTriple a1 b1 c1, Can.TTriple a2 b2 c2) ->
      unifyAll subst [(a1, a2), (b1, b2), (c1, c2)]

    (Can.TRecord f1 e1, Can.TRecord f2 e2) ->
      unifyRecords subst (Map.map fieldType f1) e1 (Map.map fieldType f2) e2

    (Can.TTagRow _ _, Can.TTagRow _ _) ->
      Just subst

    _ ->
      Nothing


unifyAll :: Subst -> [(Can.Type, Can.Type)] -> Maybe Subst
unifyAll subst pairs =
  case pairs of
    [] -> Just subst
    (a, b) : rest -> unify subst a b >>= \s -> unifyAll s rest


unifyRecords :: (Ord k) => Subst -> Map.Map k Can.Type -> Maybe T.Var -> Map.Map k Can.Type -> Maybe T.Var -> Maybe Subst
unifyRecords subst f1 e1 f2 e2 =
  let
    shared = Map.elems (Map.intersectionWith (,) f1 f2)
    only1 = Map.difference f1 f2
    only2 = Map.difference f2 f1
    allowed = (Map.null only1 || isOpen e2) && (Map.null only2 || isOpen e1)
    isOpen = maybe False (const True)
  in
  if allowed then unifyAll subst shared else Nothing


fieldType :: Can.FieldType -> Can.Type
fieldType (Can.FieldType _ t) =
  t


walk :: Subst -> Can.Type -> Can.Type
walk subst t =
  case t of
    Can.TVar x | Just bound <- Map.lookup (T.varToChars x) subst -> walk subst bound
    _ -> t


bind :: Subst -> T.Var -> Can.Type -> Maybe Subst
bind subst x t =
  if occurs subst (T.varToChars x) t || not (kindAllows (T.varToChars x) (walk subst t))
    then Nothing
    else Just (Map.insert (T.varToChars x) t subst)


occurs :: Subst -> String -> Can.Type -> Bool
occurs subst x t =
  case walk subst t of
    Can.TVar y            -> T.varToChars y == x
    Can.TLambda a b       -> occurs subst x a || occurs subst x b
    Can.TType _ _ args    -> any (occurs subst x) args
    Can.TRecord fields _  -> any (occurs subst x . fieldType) (Map.elems fields)
    Can.TPair a b         -> occurs subst x a || occurs subst x b
    Can.TTriple a b c     -> any (occurs subst x) [a, b, c]
    _                     -> False


-- What `number`, `comparable`, `appendable` and `compappend` stand for.
kindAllows :: String -> Can.Type -> Bool
kindAllows var t =
  let
    named name = case t of Can.TType _ n _ -> T.nameToChars n == name; _ -> False
    isVar = case t of Can.TVar _ -> True; _ -> False
    comparable = isVar || any named ["Int", "Float", "Char", "String", "List"] || isTuple
    isTuple = case t of Can.TPair _ _ -> True; Can.TTriple _ _ _ -> True; _ -> False
  in
  if "number" `List.isPrefixOf` var then isVar || named "Int" || named "Float"
  else if "comparable" `List.isPrefixOf` var then comparable
  else if "appendable" `List.isPrefixOf` var then isVar || named "String" || named "List"
  else if "compappend" `List.isPrefixOf` var then isVar || named "String" || named "List"
  else True


-- Keep the variables of the two types apart. The suffix keeps the kind,
-- which is read from the start of the name.
rename :: String -> Can.Type -> Can.Type
rename suffix t =
  let
    go = rename suffix
    var x = T.varFromString (S.fromChars (T.varToChars x ++ "_" ++ suffix))
  in
  case t of
    Can.TLambda a b        -> Can.TLambda (go a) (go b)
    Can.TVar x             -> Can.TVar (var x)
    Can.TType h n args     -> Can.TType h n (map go args)
    Can.TRecord fields ext -> Can.TRecord (Map.map (\(Can.FieldType i ft) -> Can.FieldType i (go ft)) fields) (fmap var ext)
    Can.TUnit              -> Can.TUnit
    Can.TPair a b          -> Can.TPair (go a) (go b)
    Can.TTriple a b c      -> Can.TTriple (go a) (go b) (go c)
    Can.TAlias h n args at -> Can.TAlias h n [ (v, go x) | (v, x) <- args ] at
    Can.TTagRow tags ext   -> Can.TTagRow (Map.map (map go) tags) (fmap var ext)
