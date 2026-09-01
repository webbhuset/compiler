{-# LANGUAGE OverloadedStrings #-}
module Type.Constrain.Expression
  ( constrain
  , constrainDef
  , constrainRecursiveDefs
  )
  where


import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name

import qualified AST.Canonical as Can
import qualified AST.Utils.Css as Css
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import Reporting.Error.Type (Expected(..), Context(..), SubContext(..), MaybeName(..), Category(..), PExpected(..), PContext(..))
import qualified Type.Constrain.Pattern as Pattern
import qualified Type.Instantiate as Instantiate
import qualified Canonicalize.Overload as CanOverload
import qualified Type.Overload as Overload
import Type.Type as Type hiding (Descriptor(..))



-- CONSTRAIN


-- As we step past type annotations, the free type variables are added to
-- the "rigid type variables" dict. Allowing sharing of rigid variables
-- between nested type annotations.
--
-- So if you have a top-level type annotation like (func : a -> b) the RTV
-- dictionary will hold variables for `a` and `b`
--
type RTV =
  Map.Map Name.Name Type


constrain :: RTV -> Can.Expr -> Expected Type -> IO Constraint
constrain rtv (A.At region expression) expected =
  case expression of
    Can.VarLocal name ->
      return (CLocal region name expected)

    Can.VarTopLevel _ name ->
      return (CLocal region name expected)

    Can.VarKernel _ _ ->
      return CTrue

    Can.VarForeign _ name annotation ->
      return $ CForeign region name annotation expected

    -- An overloaded name is typed by its abstract signature; the variable
    -- standing for this use site is handed to Type.Overload, which reads the
    -- type back once the solver has settled it and picks a definition.
    Can.VarOverload dispatch ovName annotation@(Can.Forall freeVars srcType) ->
      do  freshVars <- traverse (\_ -> mkFlexVar) freeVars
          tipe <- Instantiate.fromSrcType (Map.map VarN freshVars) srcType
          recordDispatch rtv dispatch
            [ Overload.Need ovName (freshVars Map.! var)
            | Just var <- [CanOverload.abstractVar annotation]
            ]
          return $ exists (Map.elems freshVars) $
            CEqual region (Foreign (snd ovName)) tipe expected

    -- A value whose signature has `where` clauses. Its type is instantiated
    -- here rather than by the solver so that the variables its clauses
    -- dispatch on are in hand: those are what say which definitions this
    -- reference has to be given.
    Can.VarConstrained dispatch _ name (Can.Forall freeVars srcType) constraints ->
      do  freshVars <- traverse (\_ -> mkFlexVar) freeVars
          valueType <- Instantiate.fromSrcType (Map.map VarN freshVars) srcType
          recordDispatch rtv dispatch
            [ Overload.Need ovName (freshVars Map.! CanOverload.dispatchVar c)
            | c@(Can.Constraint ovName _) <- constraints
            ]
          return $ exists (Map.elems freshVars) $
            CEqual region (Foreign name) valueType expected

    Can.VarCtor _ _ name _ annotation ->
      return $ CForeign region name annotation expected

    Can.VarTag home name params ->
      return $ CForeign region name (toTagAnnotation home name params) expected

    Can.VarDebug _ name annotation ->
      return $ CForeign region name annotation expected

    Can.VarOperator op _ _ annotation ->
      return $ CForeign region op annotation expected

    Can.Str _ ->
      return $ CEqual region String Type.string expected

    Can.Chr _ ->
      return $ CEqual region Char Type.char expected

    Can.Int _ ->
      do  var <- mkFlexNumber
          return $ exists [var] $ CEqual region E.Number (VarN var) expected

    Can.Float _ ->
      return $ CEqual region Float Type.float expected

    Can.List elements ->
      constrainList rtv region elements expected

    Can.Negate expr ->
      do  numberVar <- mkFlexNumber
          let numberType = VarN numberVar
          numberCon <- constrain rtv expr (FromContext region Negate numberType)
          let negateCon = CEqual region E.Number numberType expected
          return $ exists [numberVar] $ CAnd [ numberCon, negateCon ]

    Can.Binop op _ _ annotation leftExpr rightExpr ->
      constrainBinop rtv region op annotation leftExpr rightExpr expected

    Can.Lambda args body ->
      constrainLambda rtv region args body expected

    Can.Call func args ->
      constrainCall rtv region func args expected

    Can.If branches finally ->
      constrainIf rtv region branches finally expected

    Can.Case expr branches ->
      constrainCase rtv region expr branches expected

    Can.Let def body ->
      constrainDef rtv def
      =<< constrain rtv body expected

    Can.LetRec defs body ->
      constrainRecursiveDefs rtv defs
      =<< constrain rtv body expected

    Can.LetDestruct pattern expr body ->
      constrainDestruct rtv region pattern expr
      =<< constrain rtv body expected

    Can.Accessor field ->
      do  extVar <- mkFlexVar
          fieldVar <- mkFlexVar
          let extType = VarN extVar
          let fieldType = VarN fieldVar
          let recordType = RecordN (Map.singleton field fieldType) extType
          return $ exists [ fieldVar, extVar ] $
            CEqual region (Accessor field) (FunN recordType fieldType) expected

    Can.Access expr (A.At accessRegion field) ->
      do  extVar <- mkFlexVar
          fieldVar <- mkFlexVar
          let extType = VarN extVar
          let fieldType = VarN fieldVar
          let recordType = RecordN (Map.singleton field fieldType) extType

          let context = RecordAccess (A.toRegion expr) (getAccessName expr) accessRegion field
          recordCon <- constrain rtv expr (FromContext region context recordType)

          return $ exists [ fieldVar, extVar ] $
            CAnd
              [ recordCon
              , CEqual region (Access field) fieldType expected
              ]

    Can.Update name expr fields ->
      constrainUpdate rtv region name expr fields expected

    Can.Record fields ->
      constrainRecord rtv region fields expected

    Can.Unit ->
      return $ CEqual region Unit UnitN expected

    Can.Tuple a b maybeC ->
      constrainTuple rtv region a b maybeC expected

    Can.Shader _src types ->
      constrainShader region types expected

    Can.Css _home (Css.Content _ types) ->
      constrainCss region types expected



-- RECORDING A DISPATCH SITE
--
-- The `where` clauses in scope are paired with the rigid variable each one
-- dispatches on, which is what lets the resolver tell a use at one of them
-- from a use at some unrelated type variable.


recordDispatch :: RTV -> Can.Dispatch -> [Overload.Need] -> IO ()
recordDispatch rtv (Can.Dispatch home region clauses) needs =
  Overload.record home region needs
    [ (c, var)
    | c <- clauses
    , Just (VarN var) <- [Map.lookup (CanOverload.dispatchVar c) rtv]
    ]



-- CONSTRAIN STRUCTURAL VARIANT TAGS


-- Using a tag like `Loading` as an expression gives it the annotation
--
--     Loading : [ r | Loading ]
--
-- and a tag with arguments like `type tag Success a` gives
--
--     Success : a -> [ r | Success a ]
--
-- The row is open so that branches can add more tags.
--
toTagAnnotation :: ModuleName.Canonical -> Name.Name -> [Name.Name] -> Can.Annotation
toTagAnnotation home name params =
  let
    ext = freshExtName params
    freeVars = Map.fromList (map (\v -> (v, ())) (ext : params))
    rowType = Can.TTagRow (Map.singleton (home, name) (map Can.TVar params)) (Just ext)
    tipe = foldr (Can.TLambda . Can.TVar) rowType params
  in
  Can.Forall freeVars tipe


freshExtName :: [Name.Name] -> Name.Name
freshExtName params =
  freshExtNameHelp params (0 :: Int)


freshExtNameHelp :: [Name.Name] -> Int -> Name.Name
freshExtNameHelp params n =
  let name = if n == 0 then "r" else Name.fromTypeVariable "r" n in
  if elem name params then
    freshExtNameHelp params (n + 1)
  else
    name



-- TAG ROW CLOSING
--
-- A `case` with only tag patterns (and no catch-all) constrains its scrutinee
-- to a CLOSED row containing exactly the matched tags. This is what makes tag
-- matches exhaustive: an unhandled tag becomes a type error. The closing is
-- computed recursively, so nested tag patterns close their payload rows too.
-- A wildcard/variable pattern anywhere in a column leaves that row open.


closeTagColumn :: [Can.Pattern] -> IO (Maybe ([Variable], Type))
closeTagColumn column =
  let
    heads = map (dropAliases . A.toValue) column

    isTag p =
      case p of
        Can.PTag _ _ _ _ -> True
        _ -> False

    addGroup p groups =
      case p of
        Can.PTag home name _ args -> Map.insertWith (++) (home, name) [args] groups
        _ -> groups
  in
  if null heads then
    return Nothing
  else if all isTag heads then
    do  let groups = foldr addGroup Map.empty heads
        results <- traverse closeTagGroup groups
        let vars = Map.foldr (\(vs, _) acc -> vs ++ acc) [] results
        let tags = Map.map snd results
        return $ Just (vars, TagRowN tags EmptyTagRowN)
  else
    closeCtorColumn heads


-- A column of constructor patterns over one type, e.g. `Ok v` and
-- `Err NotFound` from a `Result`. Tags nested in a constructor argument
-- still have to be closed, or an unhandled tag would slip through, so the
-- expected type is rebuilt as `Result <closed row> a`: each of the type's
-- variables gets the closing of the sub-column of patterns bound to it.
--
-- Only arguments whose declared type IS a type variable take part;
-- Canonicalize.Pattern rejects tag patterns in any other position, so
-- there is nothing to close there.
closeCtorColumn :: [Can.Pattern_] -> IO (Maybe ([Variable], Type))
closeCtorColumn heads =
  case heads of
    Can.PCtor home typeName (Can.Union typeVars _ _ _) _ _ _ : _
      | all (isCtorOf home typeName) heads ->
          do  let argsOf p =
                    case p of
                      Can.PCtor _ _ _ _ _ args -> args
                      _ -> []

              let columnFor v =
                    [ arg
                    | p <- heads
                    , Can.PatternCtorArg _ srcType arg <- argsOf p
                    , srcType == Can.TVar v
                    ]

              results <- traverse (closeTagColumn . columnFor) typeVars

              if all Maybe.isNothing results
                then return Nothing
                else
                  do  filled <- traverse fillTypeVar results
                      return $ Just
                        ( concatMap fst filled
                        , AppN home typeName (map snd filled)
                        )

    _ ->
      return Nothing


isCtorOf :: ModuleName.Canonical -> Name.Name -> Can.Pattern_ -> Bool
isCtorOf home typeName pattern =
  case pattern of
    Can.PCtor h t _ _ _ _ -> h == home && t == typeName
    _ -> False


fillTypeVar :: Maybe ([Variable], Type) -> IO ([Variable], Type)
fillTypeVar maybeClosing =
  case maybeClosing of
    Just closing ->
      return closing

    Nothing ->
      do  var <- mkFlexVar
          return ([var], VarN var)


dropAliases :: Can.Pattern_ -> Can.Pattern_
dropAliases pattern =
  case pattern of
    Can.PAlias subPattern _ -> dropAliases (A.toValue subPattern)
    _ -> pattern


closeTagGroup :: [[Can.Pattern]] -> IO ([Variable], [Type])
closeTagGroup rows =
  case rows of
    [] ->
      return ([], [])

    firstRow : _ ->
      do  results <- traverse (\i -> closeTagArg (map (!! i) rows)) [0 .. length firstRow - 1]
          return (concatMap fst results, map snd results)


closeTagArg :: [Can.Pattern] -> IO ([Variable], Type)
closeTagArg column =
  do  maybeClosing <- closeTagColumn column
      case maybeClosing of
        Just closing ->
          return closing

        Nothing ->
          do  var <- mkFlexVar
              return ([var], VarN var)



-- CATCH-ALL NARROWING (ROW SUBTRACTION)
--
-- In `case s of Loading -> x ; other -> ...` the `other` variable can never
-- hold a Loading value at runtime, so it is bound at the scrutinee row MINUS
-- the tags matched by the earlier branches:
--
--     s ~ [ tail | Loading ]      other : tail
--
-- This is what makes row subtraction expressible:
--
--     removeLoading : r -> [ r | Loading ] -> r
--
-- Only tags that are matched IRREFUTABLY are subtracted: `Wrap (Ok n)` does
-- not consume the whole Wrap tag, since `Wrap (Err e)` values still reach the
-- catch-all branch. Tags matched with refutable arguments simply flow into
-- the tail like any other unmatched tag.


narrowTagColumn :: [Can.Pattern] -> IO (Maybe ([Variable], Type, Variable))
narrowTagColumn patterns =
  case reverse patterns of
    lastPattern : earlierRev | not (null earlierRev) ->
      let
        earlier = map (dropAliases . A.toValue) earlierRev

        isTag p =
          case p of
            Can.PTag _ _ _ _ -> True
            _ -> False

        subtractable p =
          case p of
            Can.PTag home name _ args | all isIrrefutable args ->
              Just ((home, name), length args)

            _ ->
              Nothing

        tagsToCut = Map.fromList (Maybe.mapMaybe subtractable earlier)
      in
      if not (all isTag earlier) || Map.null tagsToCut || not (bindsCatchAll lastPattern) then
        return Nothing
      else
        do  tailVar <- mkFlexVar
            tagArgs <- traverse (\arity -> traverse (\_ -> mkFlexVar) [1 .. arity]) tagsToCut
            let rowType = TagRowN (Map.map (map VarN) tagArgs) (VarN tailVar)
            let vars = tailVar : concat (Map.elems tagArgs)
            return $ Just (vars, rowType, tailVar)

    _ ->
      return Nothing


-- Is this pattern a variable (or an alias over a wildcard/variable) so that
-- narrowing actually binds something?
bindsCatchAll :: Can.Pattern -> Bool
bindsCatchAll (A.At _ pattern) =
  case pattern of
    Can.PVar _ ->
      True

    Can.PAlias subPattern _ ->
      isCatchAllShape subPattern

    _ ->
      False


isCatchAllShape :: Can.Pattern -> Bool
isCatchAllShape (A.At _ pattern) =
  case pattern of
    Can.PAnything -> True
    Can.PVar _ -> True
    Can.PAlias subPattern _ -> isCatchAllShape subPattern
    _ -> False


isIrrefutable :: Can.Pattern -> Bool
isIrrefutable (A.At _ pattern) =
  case pattern of
    Can.PAnything ->
      True

    Can.PVar _ ->
      True

    Can.PRecord _ ->
      True

    Can.PUnit ->
      True

    Can.PAlias subPattern _ ->
      isIrrefutable subPattern

    Can.PTuple a b maybeC ->
      isIrrefutable a && isIrrefutable b && maybe True isIrrefutable maybeC

    Can.PCtor _ _ (Can.Union _ _ numAlts _) _ _ args ->
      numAlts == 1 && all (\(Can.PatternCtorArg _ _ arg) -> isIrrefutable arg) args

    _ ->
      False



-- CONSTRAIN LAMBDA


constrainLambda :: RTV -> A.Region -> [Can.Pattern] -> Can.Expr -> Expected Type -> IO Constraint
constrainLambda rtv region args body expected =
  do  (Args vars tipe resultType (Pattern.State headers pvars revCons)) <-
        constrainArgs args

      bodyCon <-
        constrain rtv body (NoExpectation resultType)

      return $ exists vars $
        CAnd
          [ CLet
              { _rigidVars = []
              , _flexVars = pvars
              , _header = headers
              , _headerCon = CAnd (reverse revCons)
              , _bodyCon = bodyCon
              }
          , CEqual region Lambda tipe expected
          ]



-- CONSTRAIN CALL


constrainCall :: RTV -> A.Region -> Can.Expr -> [Can.Expr] -> Expected Type -> IO Constraint
constrainCall rtv region func@(A.At funcRegion _) args expected =
  do  let maybeName = getName func

      funcVar <- mkFlexVar
      resultVar <- mkFlexVar
      let funcType = VarN funcVar
      let resultType = VarN resultVar

      funcCon <- constrain rtv func (NoExpectation funcType)

      (argVars, argTypes, argCons) <-
        unzip3 <$> Index.indexedTraverse (constrainArg rtv region maybeName) args

      let arityType = foldr FunN resultType argTypes
      let category = CallResult maybeName

      return $ exists (funcVar:resultVar:argVars) $
        CAnd
          [ funcCon
          , CEqual funcRegion category funcType (FromContext region (CallArity maybeName (length args)) arityType)
          , CAnd argCons
          , CEqual region category resultType expected
          ]


constrainArg :: RTV -> A.Region -> MaybeName -> Index.ZeroBased -> Can.Expr -> IO (Variable, Type, Constraint)
constrainArg rtv region maybeName index arg =
  do  argVar <- mkFlexVar
      let argType = VarN argVar
      argCon <- constrain rtv arg (FromContext region (CallArg maybeName index) argType)
      return (argVar, argType, argCon)


getName :: Can.Expr -> MaybeName
getName (A.At _ expr) =
  case expr of
    Can.VarLocal name        -> FuncName name
    Can.VarTopLevel _ name   -> FuncName name
    Can.VarForeign _ name _  -> FuncName name
    Can.VarOverload _ (_, name) _ -> FuncName name
    Can.VarConstrained _ _ name _ _ -> FuncName name
    Can.VarCtor _ _ name _ _ -> CtorName name
    Can.VarOperator op _ _ _ -> OpName op
    Can.VarKernel _ name     -> FuncName name
    _                        -> NoName


getAccessName :: Can.Expr -> Maybe Name.Name
getAccessName (A.At _ expr) =
  case expr of
    Can.VarLocal name       -> Just name
    Can.VarTopLevel _ name  -> Just name
    Can.VarForeign _ name _ -> Just name
    _                       -> Nothing



-- CONSTRAIN BINOP


constrainBinop :: RTV -> A.Region -> Name.Name -> Can.Annotation -> Can.Expr -> Can.Expr -> Expected Type -> IO Constraint
constrainBinop rtv region op annotation leftExpr rightExpr expected =
  do  leftVar <- mkFlexVar
      rightVar <- mkFlexVar
      answerVar <- mkFlexVar
      let leftType = VarN leftVar
      let rightType = VarN rightVar
      let answerType = VarN answerVar
      let binopType = leftType ==> rightType ==> answerType

      let opCon = CForeign region op annotation (NoExpectation binopType)

      leftCon <- constrain rtv leftExpr (FromContext region (OpLeft op) leftType)
      rightCon <- constrain rtv rightExpr (FromContext region (OpRight op) rightType)

      return $ exists [ leftVar, rightVar, answerVar ] $
        CAnd
          [ opCon
          , leftCon
          , rightCon
          , CEqual region (CallResult (OpName op)) answerType expected
          ]



-- CONSTRAIN LISTS


constrainList :: RTV -> A.Region -> [Can.Expr] -> Expected Type -> IO Constraint
constrainList rtv region entries expected =
  do  entryVar <- mkFlexVar
      let entryType = VarN entryVar
      let listType = AppN ModuleName.list Name.list [entryType]

      entryCons <-
        Index.indexedTraverse (constrainListEntry rtv region entryType) entries

      return $ exists [entryVar] $
        CAnd
          [ CAnd entryCons
          , CEqual region List listType expected
          ]


constrainListEntry :: RTV -> A.Region -> Type -> Index.ZeroBased -> Can.Expr -> IO Constraint
constrainListEntry rtv region tipe index expr =
  constrain rtv expr (FromContext region (ListEntry index) tipe)



-- CONSTRAIN IF EXPRESSIONS


constrainIf :: RTV -> A.Region -> [(Can.Expr, Can.Expr)] -> Can.Expr -> Expected Type -> IO Constraint
constrainIf rtv region branches final expected =
  do  let boolExpect = FromContext region IfCondition Type.bool
      let (conditions, exprs) = foldr (\(c,e) (cs,es) -> (c:cs,e:es)) ([],[final]) branches

      condCons <-
        traverse (\c -> constrain rtv c boolExpect) conditions

      case expected of
        FromAnnotation name arity _ tipe ->
          do  branchCons <- Index.indexedForA exprs $ \index expr ->
                constrain rtv expr (FromAnnotation name arity (TypedIfBranch index) tipe)
              return $
                CAnd
                  [ CAnd condCons
                  , CAnd branchCons
                  ]

        _ ->
          do  branchVar <- mkFlexVar
              let branchType = VarN branchVar

              branchCons <- Index.indexedForA exprs $ \index expr ->
                constrain rtv expr (FromContext region (IfBranch index) branchType)

              return $ exists [branchVar] $
                CAnd
                  [ CAnd condCons
                  , CAnd branchCons
                  , CEqual region If branchType expected
                  ]



-- CONSTRAIN CASE EXPRESSIONS


constrainCase :: RTV -> A.Region -> Can.Expr -> [Can.CaseBranch] -> Expected Type -> IO Constraint
constrainCase rtv region expr branches expected =
  do  ptrnVar <- mkFlexVar
      let ptrnType = VarN ptrnVar
      exprCon <- constrain rtv expr (NoExpectation ptrnType)

      let patterns = map (\(Can.CaseBranch p _) -> p) branches
      maybeClosing <- closeTagColumn patterns

      (rowVars, narrowCons, closingCons, maybeTailType) <-
        case maybeClosing of
          Just (cvars, closedType) ->
            return
              ( cvars
              , []
              , [CPattern region E.PTags closedType (PFromContext region E.PCaseTags ptrnType)]
              , Nothing
              )

          Nothing ->
            do  maybeNarrowing <- narrowTagColumn patterns
                case maybeNarrowing of
                  Nothing ->
                    return ([], [], [], Nothing)

                  Just (nvars, rowType, tailVar) ->
                    return
                      ( nvars
                      , [CPattern region E.PTags rowType (PNoExpectation ptrnType)]
                      , []
                      , Just (VarN tailVar)
                      )

      let numBranches = length branches

      -- the catch-all branch (if narrowing applies) matches the scrutinee row
      -- minus the subtracted tags
      let branchPatternType index =
            case maybeTailType of
              Just tailType | Index.toHuman index == numBranches -> tailType
              _ -> ptrnType

      case expected of
        FromAnnotation name arity _ tipe ->
          do  branchCons <- Index.indexedForA branches $ \index branch ->
                constrainCaseBranch rtv branch
                  (PFromContext region (PCaseMatch index) (branchPatternType index))
                  (FromAnnotation name arity (TypedCaseBranch index) tipe)

              return $ exists (ptrnVar:rowVars) $ CAnd (exprCon : narrowCons ++ branchCons ++ closingCons)

        _ ->
          do  branchVar <- mkFlexVar
              let branchType = VarN branchVar

              branchCons <- Index.indexedForA branches $ \index branch ->
                constrainCaseBranch rtv branch
                  (PFromContext region (PCaseMatch index) (branchPatternType index))
                  (FromContext region (CaseBranch index) branchType)

              return $ exists (ptrnVar:branchVar:rowVars) $
                CAnd
                  [ exprCon
                  , CAnd narrowCons
                  , CAnd branchCons
                  , CAnd closingCons
                  , CEqual region Case branchType expected
                  ]


constrainCaseBranch :: RTV -> Can.CaseBranch -> PExpected Type -> Expected Type -> IO Constraint
constrainCaseBranch rtv (Can.CaseBranch pattern expr) pExpect bExpect =
  do  (Pattern.State headers pvars revCons) <-
        Pattern.add pattern pExpect Pattern.emptyState

      CLet [] pvars headers (CAnd (reverse revCons))
        <$> constrain rtv expr bExpect



-- CONSTRAIN RECORD


constrainRecord :: RTV -> A.Region -> Map.Map Name.Name Can.Expr -> Expected Type -> IO Constraint
constrainRecord rtv region fields expected =
  do  dict <- traverse (constrainField rtv) fields

      let getType (_, t, _) = t
      let recordType = RecordN (Map.map getType dict) EmptyRecordN
      let recordCon = CEqual region Record recordType expected

      let vars = Map.foldr (\(v,_,_) vs -> v:vs) [] dict
      let cons = Map.foldr (\(_,_,c) cs -> c:cs) [recordCon] dict

      return $ exists vars (CAnd cons)


constrainField :: RTV -> Can.Expr -> IO (Variable, Type, Constraint)
constrainField rtv expr =
  do  var <- mkFlexVar
      let tipe = VarN var
      con <- constrain rtv expr (NoExpectation tipe)
      return (var, tipe, con)



-- CONSTRAIN RECORD UPDATE


constrainUpdate :: RTV -> A.Region -> Name.Name -> Can.Expr -> Map.Map Name.Name Can.FieldUpdate -> Expected Type -> IO Constraint
constrainUpdate rtv region name expr fields expected =
  do  extVar <- mkFlexVar
      fieldDict <- Map.traverseWithKey (constrainUpdateField rtv region) fields

      recordVar <- mkFlexVar
      let recordType = VarN recordVar
      let fieldsType = RecordN (Map.map (\(_,t,_) -> t) fieldDict) (VarN extVar)

      -- NOTE: fieldsType is separate so that Error propagates better
      let fieldsCon = CEqual region Record recordType (NoExpectation fieldsType)
      let recordCon = CEqual region Record recordType expected

      let vars = Map.foldr (\(v,_,_) vs -> v:vs) [recordVar,extVar] fieldDict
      let cons = Map.foldr (\(_,_,c) cs -> c:cs) [recordCon] fieldDict

      con <- constrain rtv expr (FromContext region (RecordUpdateKeys name fields) recordType)

      return $ exists vars $ CAnd (fieldsCon:con:cons)


constrainUpdateField :: RTV -> A.Region -> Name.Name -> Can.FieldUpdate -> IO (Variable, Type, Constraint)
constrainUpdateField rtv region field (Can.FieldUpdate _ expr) =
  do  var <- mkFlexVar
      let tipe = VarN var
      con <- constrain rtv expr (FromContext region (RecordUpdateValue field) tipe)
      return (var, tipe, con)



-- CONSTRAIN TUPLE


constrainTuple :: RTV -> A.Region -> Can.Expr -> Can.Expr -> Maybe Can.Expr -> Expected Type -> IO Constraint
constrainTuple rtv region a b maybeC expected =
  do  aVar <- mkFlexVar
      bVar <- mkFlexVar
      let aType = VarN aVar
      let bType = VarN bVar

      aCon <- constrain rtv a (NoExpectation aType)
      bCon <- constrain rtv b (NoExpectation bType)

      case maybeC of
        Nothing ->
          do  let tupleType = TupleN aType bType Nothing
              let tupleCon = CEqual region Tuple tupleType expected
              return $ exists [ aVar, bVar ] $ CAnd [ aCon, bCon, tupleCon ]

        Just c ->
          do  cVar <- mkFlexVar
              let cType = VarN cVar

              cCon <- constrain rtv c (NoExpectation cType)

              let tupleType = TupleN aType bType (Just cType)
              let tupleCon = CEqual region Tuple tupleType expected

              return $ exists [ aVar, bVar, cVar ] $ CAnd [ aCon, bCon, cCon, tupleCon ]



-- CONSTRAIN SHADER


constrainShader :: A.Region -> Shader.Types -> Expected Type -> IO Constraint
constrainShader region (Shader.Types attributes uniforms varyings) expected =
  do  attrVar <- mkFlexVar
      unifVar <- mkFlexVar
      let attrType = VarN attrVar
      let unifType = VarN unifVar

      let shaderType =
            AppN ModuleName.webgl Name.shader
              [ toShaderRecord attributes attrType
              , toShaderRecord uniforms unifType
              , toShaderRecord varyings EmptyRecordN
              ]

      return $ exists [ attrVar, unifVar ] $
        CEqual region Shader shaderType expected


toShaderRecord :: Map.Map Name.Name Shader.Type -> Type -> Type
toShaderRecord types baseRecType =
  if Map.null types then
    baseRecType
  else
    RecordN (Map.map glToType types) baseRecType


glToType :: Shader.Type -> Type
glToType glType =
  case glType of
    Shader.V2 -> Type.vec2
    Shader.V3 -> Type.vec3
    Shader.V4 -> Type.vec4
    Shader.M4 -> Type.mat4
    Shader.Int -> Type.int
    Shader.Float -> Type.float
    Shader.Texture -> Type.texture



-- CONSTRAIN CSS


constrainCss :: A.Region -> Css.Types -> Expected Type -> IO Constraint
constrainCss region (Css.Types classes keyframes vars) expected =
  let
    classFields =
      Map.union
        (Map.fromSet (const (cssAppN "Class")) classes)
        (Map.fromSet (const (cssAppN "Animation")) keyframes)

    varFields =
      Map.map propToType vars

    cssType =
      AppN ModuleName.cssStyles (Name.fromChars "Stylesheet")
        [ toCssRecord classFields
        , toCssRecord varFields
        ]
  in
  return (CEqual region CssBlock cssType expected)


toCssRecord :: Map.Map Name.Name Type -> Type
toCssRecord fields =
  if Map.null fields then
    EmptyRecordN
  else
    RecordN fields EmptyRecordN


propToType :: Css.PropType -> Type
propToType propType =
  case propType of
    Css.Value      -> cssAppN "Value"
    Css.Length     -> cssAppN "Length"
    Css.Percentage -> cssAppN "Percentage"
    Css.Color      -> cssAppN "Color"
    Css.Number     -> Type.float
    Css.Integer    -> Type.int
    Css.Duration   -> cssAppN "Duration"
    Css.Angle      -> cssAppN "Angle"


cssAppN :: [Char] -> Type
cssAppN name =
  AppN ModuleName.cssStyles (Name.fromChars name) []



-- CONSTRAIN DESTRUCTURES


constrainDestruct :: RTV -> A.Region -> Can.Pattern -> Can.Expr -> Constraint -> IO Constraint
constrainDestruct rtv region pattern expr bodyCon =
  do  patternVar <- mkFlexVar
      let patternType = VarN patternVar

      state <-
        addTagClosing region patternType [pattern] =<<
          Pattern.add pattern (PNoExpectation patternType) Pattern.emptyState

      let (Pattern.State headers pvars revCons) = state

      exprCon <-
        constrain rtv expr (FromContext region Destructure patternType)

      return $ CLet [] (patternVar:pvars) headers (CAnd (reverse (exprCon:revCons))) bodyCon


-- Close the variant row when a tag pattern appears in a position that must
-- be irrefutable (function arguments and `let` destructuring).
addTagClosing :: A.Region -> Type -> [Can.Pattern] -> Pattern.State -> IO Pattern.State
addTagClosing region tipe column state@(Pattern.State headers vars revCons) =
  do  maybeClosing <- closeTagColumn column
      case maybeClosing of
        Nothing ->
          return state

        Just (cvars, closedType) ->
          return $ Pattern.State
            headers
            (cvars ++ vars)
            (CPattern region E.PTags closedType (PNoExpectation tipe) : revCons)



-- CONSTRAIN DEF


constrainDef :: RTV -> Can.Def -> Constraint -> IO Constraint
constrainDef rtv def bodyCon =
  case def of
    Can.Def (A.At region name) args expr ->
      do  (Args vars tipe resultType (Pattern.State headers pvars revCons)) <-
            constrainArgs args

          exprCon <-
            constrain rtv expr (NoExpectation resultType)

          return $
            CLet
              { _rigidVars = []
              , _flexVars = vars
              , _header = Map.singleton name (A.At region tipe)
              , _headerCon =
                  CLet
                    { _rigidVars = []
                    , _flexVars = pvars
                    , _header = headers
                    , _headerCon = CAnd (reverse revCons)
                    , _bodyCon = exprCon
                    }
              , _bodyCon = bodyCon
              }

    Can.TypedDef (A.At region name) freeVars typedArgs expr srcResultType ->
      do  let newNames = Map.difference freeVars rtv
          newRigids <- Map.traverseWithKey (\n _ -> nameToRigid n) newNames
          let newRtv = Map.union rtv (Map.map VarN newRigids)

          (TypedArgs tipe resultType (Pattern.State headers pvars revCons)) <-
            constrainTypedArgs newRtv name typedArgs srcResultType

          let expected = FromAnnotation name (length typedArgs) TypedBody resultType
          exprCon <-
            constrain newRtv expr expected

          return $
            CLet
              { _rigidVars = Map.elems newRigids
              , _flexVars = []
              , _header = Map.singleton name (A.At region tipe)
              , _headerCon =
                  CLet
                    { _rigidVars = []
                    , _flexVars = pvars
                    , _header = headers
                    , _headerCon = CAnd (reverse revCons)
                    , _bodyCon = exprCon
                    }
              , _bodyCon = bodyCon
              }



-- CONSTRAIN RECURSIVE DEFS


data Info =
  Info
    { _vars :: [Variable]
    , _cons :: [Constraint]
    , _headers :: Map.Map Name.Name (A.Located Type)
    }


{-# NOINLINE emptyInfo #-}
emptyInfo :: Info
emptyInfo =
  Info [] [] Map.empty


constrainRecursiveDefs :: RTV -> [Can.Def] -> Constraint -> IO Constraint
constrainRecursiveDefs rtv defs bodyCon =
  recDefsHelp rtv defs bodyCon emptyInfo emptyInfo


recDefsHelp :: RTV -> [Can.Def] -> Constraint -> Info -> Info -> IO Constraint
recDefsHelp rtv defs bodyCon rigidInfo flexInfo =
  case defs of
    [] ->
      do  let (Info rigidVars rigidCons rigidHeaders) = rigidInfo
          let (Info flexVars  flexCons  flexHeaders ) = flexInfo
          return $
            CLet rigidVars [] rigidHeaders CTrue $
              CLet [] flexVars flexHeaders (CLet [] [] flexHeaders CTrue (CAnd flexCons)) $
                CAnd [ CAnd rigidCons, bodyCon ]

    def : otherDefs ->
      case def of
        Can.Def (A.At region name) args expr ->
          do  let (Info flexVars flexCons flexHeaders) = flexInfo

              (Args newFlexVars tipe resultType (Pattern.State headers pvars revCons)) <-
                argsHelp args (Pattern.State Map.empty flexVars [])

              exprCon <-
                constrain rtv expr (NoExpectation resultType)

              let defCon =
                    CLet
                      { _rigidVars = []
                      , _flexVars = pvars
                      , _header = headers
                      , _headerCon = CAnd (reverse revCons)
                      , _bodyCon = exprCon
                      }

              recDefsHelp rtv otherDefs bodyCon rigidInfo $
                Info
                  { _vars = newFlexVars
                  , _cons = defCon : flexCons
                  , _headers = Map.insert name (A.At region tipe) flexHeaders
                  }

        Can.TypedDef (A.At region name) freeVars typedArgs expr srcResultType ->
          do  let newNames = Map.difference freeVars rtv
              newRigids <- Map.traverseWithKey (\n _ -> nameToRigid n) newNames
              let newRtv = Map.union rtv (Map.map VarN newRigids)

              (TypedArgs tipe resultType (Pattern.State headers pvars revCons)) <-
                constrainTypedArgs newRtv name typedArgs srcResultType

              exprCon <-
                constrain newRtv expr $
                  FromAnnotation name (length typedArgs) TypedBody resultType

              let defCon =
                    CLet
                      { _rigidVars = []
                      , _flexVars = pvars
                      , _header = headers
                      , _headerCon = CAnd (reverse revCons)
                      , _bodyCon = exprCon
                      }

              let (Info rigidVars rigidCons rigidHeaders) = rigidInfo
              recDefsHelp rtv otherDefs bodyCon
                ( Info
                    { _vars = Map.foldr (:) rigidVars newRigids
                    , _cons = CLet (Map.elems newRigids) [] Map.empty defCon CTrue : rigidCons
                    , _headers = Map.insert name (A.At region tipe) rigidHeaders
                    }
                )
                flexInfo



-- CONSTRAIN ARGS


data Args =
  Args
    { _a_vars :: [Variable]
    , _a_type :: Type
    , _a_result :: Type
    , _a_state :: Pattern.State
    }


constrainArgs :: [Can.Pattern] -> IO Args
constrainArgs args =
  argsHelp args Pattern.emptyState


argsHelp :: [Can.Pattern] -> Pattern.State -> IO Args
argsHelp args state =
  case args of
    [] ->
      do  resultVar <- mkFlexVar
          let resultType = VarN resultVar
          return $ Args [resultVar] resultType resultType state

    pattern@(A.At region _) : otherArgs ->
      do  argVar <- mkFlexVar
          let argType = VarN argVar

          (Args vars tipe result newState) <-
            argsHelp otherArgs =<<
              addTagClosing region argType [pattern] =<<
                Pattern.add pattern (PNoExpectation argType) state

          return (Args (argVar:vars) (FunN argType tipe) result newState)



-- CONSTRAIN TYPED ARGS


data TypedArgs =
  TypedArgs
    { _t_type :: Type
    , _t_result :: Type
    , _t_state :: Pattern.State
    }


constrainTypedArgs :: Map.Map Name.Name Type -> Name.Name -> [(Can.Pattern, Can.Type)] -> Can.Type -> IO TypedArgs
constrainTypedArgs rtv name args srcResultType =
  typedArgsHelp rtv name Index.first args srcResultType Pattern.emptyState


typedArgsHelp :: Map.Map Name.Name Type -> Name.Name -> Index.ZeroBased -> [(Can.Pattern, Can.Type)] -> Can.Type -> Pattern.State -> IO TypedArgs
typedArgsHelp rtv name index args srcResultType state =
  case args of
    [] ->
      do  resultType <- Instantiate.fromSrcType rtv srcResultType
          return $ TypedArgs resultType resultType state

    (pattern@(A.At region _), srcType) : otherArgs ->
      do  argType <- Instantiate.fromSrcType rtv srcType
          let expected = PFromContext region (PTypedArg name index) argType

          (TypedArgs tipe resultType newState) <-
            typedArgsHelp rtv name (Index.next index) otherArgs srcResultType =<<
              addTagClosing region argType [pattern] =<<
                Pattern.add pattern expected state

          return (TypedArgs (FunN argType tipe) resultType newState)
