module AST.Utils.Type
  ( delambda
  , dealias
  , deepDealias
  , iteratedDealias
  )
  where


import qualified Data.Map as Map

import AST.Canonical (Type(..), AliasType(..), FieldType(..))
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeVar as T



-- DELAMBDA


delambda :: Type -> [Type]
delambda tipe =
  case tipe of
    TLambda arg result ->
      arg : delambda result

    _ ->
      [tipe]



-- DEALIAS


dealias :: [(T.Var, Type)] -> AliasType -> Type
dealias args aliasType =
  case aliasType of
    Holey  tipe -> dealiasHelp (Map.fromList args) tipe
    Filled tipe -> tipe


dealiasHelp :: Map.Map T.Var Type -> Type -> Type
dealiasHelp typeTable =
    go
  where
    go tipe =
      case tipe of
        TLambda a b     -> TLambda (go a) (go b)
        TVar x          -> Map.findWithDefault tipe x typeTable
        TRecord fs e    -> dealiasRecord typeTable (Map.map (dealiasField typeTable) fs) e
        TAlias h n xs t -> TAlias h n (map (fmap go) xs) t
        TType  h n xs   -> TType  h n (map go xs)
        TUnit           -> TUnit
        TPair   a b     -> TPair (go a) (go b)
        TTriple a b c   -> TTriple (go a) (go b) (go c)
        TTagRow ts e    -> TTagRow (Map.map (map go) ts) e


dealiasRecord :: Map.Map T.Var Type -> Map.Map N.Name FieldType -> Maybe T.Var -> Type
dealiasRecord typeTable fields ext =
  case ext >>= \x -> Map.lookup x typeTable of
    Nothing ->
      TRecord fields ext

    Just extType ->
      case iteratedDealias extType of
        TRecord subFields subExt -> TRecord (Map.union subFields fields) subExt
        TVar x                   -> TRecord fields (Just x)
        _                        -> TRecord fields ext


dealiasField :: Map.Map T.Var Type -> FieldType -> FieldType
dealiasField typeTable (FieldType index tipe) =
  FieldType index (dealiasHelp typeTable tipe)



-- DEEP DEALIAS


deepDealias :: Type -> Type
deepDealias tipe =
  case tipe of
    TLambda a b     -> TLambda (deepDealias a) (deepDealias b)
    TVar _          -> tipe
    TRecord fs x    -> TRecord (Map.map deepDealiasField fs) x
    TAlias _ _ xs t -> deepDealias (dealias xs t)
    TType h n xs    -> TType h n (map deepDealias xs)
    TUnit           -> TUnit
    TPair   a b     -> TPair (deepDealias a) (deepDealias b)
    TTriple a b c   -> TTriple (deepDealias a) (deepDealias b) (deepDealias c)

    TTagRow tags ext ->
      TTagRow (Map.map (map deepDealias) tags) ext


deepDealiasField :: FieldType -> FieldType
deepDealiasField (FieldType index tipe) =
  FieldType index (deepDealias tipe)



-- ITERATED DEALIAS


iteratedDealias :: Type -> Type
iteratedDealias tipe =
  case tipe of
    TAlias _ _ xs t -> iteratedDealias (dealias xs t)
    _               -> tipe
