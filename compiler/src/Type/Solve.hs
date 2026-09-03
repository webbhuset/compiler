{-# LANGUAGE OverloadedStrings #-}
module Type.Solve
  ( run
  )
  where


import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L
import qualified Type.Instantiate as Instantiate
import qualified Type.Occurs as Occurs
import qualified Type.Overload as Overload
import Type.Type as Type
import qualified Type.Error as ET
import qualified Type.Unify as Unify
import qualified Type.UnionFind as UF



-- RUN SOLVER


run :: Can.Overloads -> Constraint -> IO (Either (NE.List Error.Error) (Map.Map Name.Name Can.Annotation))
run overloads constraint =
  do  pools <- MVector.replicate 8 []

      (State env _ errors _ _ _) <-
        settleWidens outermostRank pools =<<
          settlePending outermostRank pools =<<
            solve Map.empty outermostRank pools (emptyState overloads) constraint

      case errors of
        [] ->
          Right <$> traverse Type.toAnnotation env

        e:es ->
          return $ Left (NE.List e es)



emptyState :: Can.Overloads -> State
emptyState overloads =
  State Map.empty (nextMark noMark) [] overloads [] []



-- SOLVER


type Env =
  Map.Map Name.Name Variable


type Pools =
  MVector.IOVector [Variable]


data State =
  State
    { _env :: Env
    , _mark :: Mark
    , _errors :: [Error.Error]
    , _overloads :: Can.Overloads
    -- Overload uses whose dispatch type has not settled yet, each with the
    -- `where` clauses that were in scope. Looked at before a definition
    -- generalizes; see settlePending.
    , _pending :: [(Need, [Clause])]
    -- `widen` sites whose row inclusion is still to be checked, most recent
    -- first. Settled before a definition generalizes; see settleWidens.
    , _widens :: [Widen]
    }


-- The source row (type of the widened expression) and the target row (what
-- the context expects) of one `widen` site.
data Widen =
  Widen A.Region Variable Variable


solve :: Env -> Int -> Pools -> State -> Constraint -> IO State
solve env rank pools state constraint =
  case constraint of
    CTrue ->
      return state

    CSaveTheEnvironment ->
      return (state { _env = env })

    CEqual region category tipe expectation ->
      do  actual <- typeToVariable rank pools tipe
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region category actualType $
                      Error.typeReplace expectation expectedType

    CLocal region name expectation ->
      do  actual <- makeCopy rank pools (env ! name)
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region (Error.Local name) actualType $
                      Error.typeReplace expectation expectedType

    CForeign region name (Can.Forall freeVars srcType) expectation ->
      do  actual <- srcTypeToVariable rank pools freeVars srcType
          expected <- expectedToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadExpr region (Error.Foreign name) actualType $
                      Error.typeReplace expectation expectedType

    CPattern region category tipe expectation ->
      do  actual <- typeToVariable rank pools tipe
          expected <- patternExpectationToVariable rank pools expectation
          answer <- Unify.unify actual expected
          case answer of
            Unify.Ok vars ->
              do  introduce rank pools vars
                  return state

            Unify.Err vars actualType expectedType ->
              do  introduce rank pools vars
                  return $ addError state $
                    Error.BadPattern region category actualType
                      (Error.ptypeReplace expectation expectedType)

    CAnd constraints ->
      foldM (solve env rank pools) state constraints

    CDispatch needs clauses ->
      return state { _pending = [ (need, clauses) | need <- needs ] ++ _pending state }

    CWiden region sourceType targetType ->
      do  source <- typeToVariable rank pools sourceType
          target <- typeToVariable rank pools targetType
          return state { _widens = Widen region source target : _widens state }

    CLet [] flexs _ headerCon CTrue ->
      do  introduce rank pools flexs
          solve env rank pools state headerCon

    CLet [] [] header headerCon subCon ->
      do  state1 <- settleWidens rank pools =<< settlePending rank pools =<< solve env rank pools state headerCon
          locals <- traverse (A.traverse (typeToVariable rank pools)) header
          let newEnv = Map.union env (Map.map A.toValue locals)
          state2 <- solve newEnv rank pools state1 subCon
          foldM occurs state2 $ Map.toList locals

    CLet rigids flexs header headerCon subCon ->
      do
          -- work in the next pool to localize header
          let nextRank = rank + 1
          let poolsLength = MVector.length pools
          nextPools <-
            if nextRank < poolsLength
              then return pools
              else MVector.grow pools poolsLength

          -- introduce variables
          let vars = rigids ++ flexs
          forM_ vars $ \var ->
            UF.modify var $ \(Descriptor content _ mark copy) ->
              Descriptor content nextRank mark copy
          MVector.write nextPools nextRank vars

          -- run solver in next pool
          locals <- traverse (A.traverse (typeToVariable nextRank nextPools)) header
          (State savedEnv mark errors overloads pending widens) <-
            settleWidens nextRank nextPools =<<
              settlePending nextRank nextPools =<<
                solve env nextRank nextPools state headerCon

          let youngMark = mark
          let visitMark = nextMark youngMark
          let finalMark = nextMark visitMark

          -- pop pool
          generalize youngMark visitMark nextRank nextPools
          MVector.write nextPools nextRank []

          -- check that things went well
          mapM_ isGeneric rigids

          let newEnv = Map.union env (Map.map A.toValue locals)
          let tempState = State savedEnv finalMark errors overloads pending widens
          newState <- solve newEnv rank nextPools tempState subCon

          foldM occurs newState (Map.toList locals)


-- SETTLING WIDEN SITES
--
-- `widen e` is sound when every tag of e's row is in the target row with the
-- same payload, and the rows agree on their remainder: the source is closed,
-- or both end in the same variable. That is an inclusion, not an equation,
-- so it cannot be a unification. It is checked here, just before the
-- enclosing definition generalizes, once both rows are as solved as they
-- will get. Inclusion is stable under instantiation (the same tail variable
-- stays the same variable in every copy), so checking once is enough.
--
-- A source whose tail is still an unconstrained flex variable is widened by
-- plain unification instead: nothing constrained it, so `widen` should behave
-- as if it were not there rather than fail as ambiguous.


settleWidens :: Int -> Pools -> State -> IO State
settleWidens rank pools state =
  do  state1 <- foldM (settleWiden rank pools) state (reverse (_widens state))
      return state1 { _widens = [] }


settleWiden :: Int -> Pools -> State -> Widen -> IO State
settleWiden rank pools state (Widen region source target) =
  do  sourceRow <- gatherRow source
      targetRow <- gatherRow target
      case (sourceRow, targetRow) of
        (RowFlex, _) ->
          widenByUnification rank pools state region source target

        (_, RowFlex) ->
          widenByUnification rank pools state region source target

        (RowNotVariant, _) ->
          widenError state region Error.WidenSourceNotVariant source target

        (_, RowNotVariant) ->
          widenError state region Error.WidenTargetNotVariant source target

        (Row sourceTags sourceTail, Row targetTags targetTail) ->
          do  tailOk <- checkTails rank pools sourceTail targetTail
              case tailOk of
                TailsFlex ->
                  widenByUnification rank pools state region source target

                TailsDiffer ->
                  widenError state region Error.WidenRemainders source target

                TailsAgree ->
                  let
                    missing =
                      Map.keys (Map.difference sourceTags targetTags)
                  in
                  case missing of
                    _ : _ ->
                      widenError state region (Error.WidenMissingTags (map snd missing)) source target

                    [] ->
                      do  -- render the rows now: a failing payload unification
                          -- would otherwise turn them into error types
                          sourceType <- Type.toErrorType source
                          targetType <- Type.toErrorType target
                          foldM (checkPayload rank pools region sourceType targetType targetTags) state (Map.toList sourceTags)


data RowInfo
  = Row (Map.Map Can.TagKey [Variable]) Variable
  | RowFlex
  | RowNotVariant


-- Flatten a variant row the way Type.Unify does before comparing rows: walk
-- the chain of TagRow1 extensions and aliases, collecting tags until the tail.
gatherRow :: Variable -> IO RowInfo
gatherRow variable =
  gatherRowHelp Map.empty variable


gatherRowHelp :: Map.Map Can.TagKey [Variable] -> Variable -> IO RowInfo
gatherRowHelp tags variable =
  do  (Descriptor content _ _ _) <- UF.get variable
      case content of
        Structure (TagRow1 subTags subExt) ->
          gatherRowHelp (Map.union tags subTags) subExt

        Structure EmptyTagRow1 ->
          return (Row tags variable)

        Alias _ _ _ realVar ->
          gatherRowHelp tags realVar

        FlexVar _ ->
          if Map.null tags then return RowFlex else return (Row tags variable)

        RigidVar _ ->
          return (Row tags variable)

        FlexSuper _ _ ->
          return RowNotVariant

        RigidSuper _ _ ->
          return RowNotVariant

        Structure _ ->
          return RowNotVariant

        Error ->
          return RowNotVariant


data Tails
  = TailsAgree
  | TailsFlex
  | TailsDiffer


-- A closed source fits under any remainder. Otherwise the tails must be the
-- same variable; a flex target tail is bound to the source tail to make it
-- so, and a flex source tail means the whole site is better handled by
-- unification.
checkTails :: Int -> Pools -> Variable -> Variable -> IO Tails
checkTails rank pools sourceTail targetTail =
  do  (Descriptor sourceContent _ _ _) <- UF.get sourceTail
      case sourceContent of
        Structure EmptyTagRow1 ->
          return TailsAgree

        FlexVar _ ->
          return TailsFlex

        _ ->
          do  same <- UF.equivalent sourceTail targetTail
              if same
                then return TailsAgree
                else
                  do  (Descriptor targetContent _ _ _) <- UF.get targetTail
                      case targetContent of
                        FlexVar _ ->
                          do  answer <- Unify.unify sourceTail targetTail
                              case answer of
                                Unify.Ok vars ->
                                  do  introduce rank pools vars
                                      return TailsAgree

                                Unify.Err vars _ _ ->
                                  do  introduce rank pools vars
                                      return TailsDiffer

                        _ ->
                          return TailsDiffer


checkPayload :: Int -> Pools -> A.Region -> ET.Type -> ET.Type -> Map.Map Can.TagKey [Variable] -> State -> (Can.TagKey, [Variable]) -> IO State
checkPayload rank pools region sourceType targetType targetTags state (key@(_, tagName), sourceArgs) =
  case Map.lookup key targetTags of
    Nothing ->
      return state  -- already reported as missing

    Just targetArgs ->
      if length sourceArgs /= length targetArgs then
        return $ addError state $
          Error.BadWiden region (Error.WidenMissingTags [tagName]) sourceType targetType
      else
        foldM (checkPayloadArg rank pools region tagName sourceType targetType) state (zip sourceArgs targetArgs)


checkPayloadArg :: Int -> Pools -> A.Region -> Name.Name -> ET.Type -> ET.Type -> State -> (Variable, Variable) -> IO State
checkPayloadArg rank pools region tagName sourceType targetType state (sourceArg, targetArg) =
  do  answer <- Unify.unify sourceArg targetArg
      case answer of
        Unify.Ok vars ->
          do  introduce rank pools vars
              return state

        Unify.Err vars actualType expectedType ->
          do  introduce rank pools vars
              return $ addError state $
                Error.BadWiden region (Error.WidenPayload tagName actualType expectedType) sourceType targetType


widenByUnification :: Int -> Pools -> State -> A.Region -> Variable -> Variable -> IO State
widenByUnification rank pools state region source target =
  do  answer <- Unify.unify source target
      case answer of
        Unify.Ok vars ->
          do  introduce rank pools vars
              return state

        Unify.Err vars actualType expectedType ->
          do  introduce rank pools vars
              return $ addError state $
                Error.BadWiden region Error.WidenMismatch actualType expectedType


widenError :: State -> A.Region -> Error.WidenProblem -> Variable -> Variable -> IO State
widenError state region problem source target =
  do  sourceType <- Type.toErrorType source
      targetType <- Type.toErrorType target
      return $ addError state (Error.BadWiden region problem sourceType targetType)



-- SETTLING OVERLOAD USES
--
-- A `where` clause is invisible to the type checker, so on its own the
-- checker cannot know that in `minimum : t -> Maybe item` the `item` is fixed
-- by whichever definition `t` picks. Left alone it would generalize `item`,
-- and `minimum "caf"` would come out as `Maybe a` while holding a Char.
--
-- So just before a definition generalizes, every overload use recorded inside
-- it whose dispatch type has settled is looked up, and the rest of its clause
-- is unified with the definition found. That pins `item` to `Char` while it
-- still can be pinned. Uses that have not settled stay pending for an outer
-- definition, and whatever is left at the end is reported by Type.Overload.


settlePending :: Int -> Pools -> State -> IO State
settlePending rank pools state =
  do  (remaining, progressed, state1) <-
        foldM (settleOne rank pools) ([], False, state) (_pending state)
      let state2 = state1 { _pending = remaining }
      -- pinning one variable can settle another use's dispatch type
      if progressed then settlePending rank pools state2 else return state2


settleOne
  :: Int -> Pools
  -> ([(Need, [Clause])], Bool, State)
  -> (Need, [Clause])
  -> IO ([(Need, [Clause])], Bool, State)
settleOne rank pools (remaining, progressed, state) entry@(need, clauses) =
  do  outcome <- trySettle rank pools (_overloads state) need clauses
      case outcome of
        Pending ->
          return (entry : remaining, progressed, state)

        Settled ->
          return (remaining, True, state)

        Clash err ->
          return (remaining, True, addError state err)


data Outcome
  = Pending
  | Settled
  | Clash Error.Error


trySettle :: Int -> Pools -> Can.Overloads -> Need -> [Clause] -> IO Outcome
trySettle rank pools overloads (Need region ovName dispatchVar clauseType vars) clauses =
  do  (Descriptor content _ _ _) <- UF.get dispatchVar
      case content of
        FlexVar _ ->
          return Pending

        FlexSuper _ _ ->
          return Pending

        -- a `where` clause of the enclosing definition, if there is one for
        -- this name at this variable: the clause is the definition here
        RigidVar _ ->
          do  matches <- filterM (isClauseFor ovName dispatchVar) clauses
              case matches of
                Clause (Can.Constraint _ enclosingType) _ enclosingVars : _ ->
                  do  enclosing <- typeToVariable rank pools =<< Instantiate.fromSrcType (Map.map VarN enclosingVars) enclosingType
                      unifyClause rank pools region ovName clauseType vars enclosing

                [] ->
                  return Settled   -- no clause: Type.Overload reports it

        RigidSuper _ _ ->
          return Settled           -- comparable and friends: Type.Overload reports it

        Error ->
          return Settled

        _ ->
          do  Can.Forall _ dispatched <- Type.toAnnotation dispatchVar
              case Overload.lookupDefinition overloads ovName dispatched of
                Nothing ->
                  return Settled   -- no definition: Type.Overload reports it

                Just (Can.Forall declaredFree declared) ->
                  do  definition <- srcTypeToVariable rank pools declaredFree declared
                      unifyClause rank pools region ovName clauseType vars definition


isClauseFor :: Can.OverloadName -> Variable -> Clause -> IO Bool
isClauseFor ovName dispatchVar (Clause (Can.Constraint name _) clauseDispatch _) =
  if name /= ovName then return False else UF.equivalent dispatchVar clauseDispatch


-- The clause as instantiated at this use, against what actually provides it.
-- Structurally these always agree, since both are the abstract signature with
-- types filled in, so what this really does is copy the definition's choices
-- into the use site's still-open variables.
unifyClause :: Int -> Pools -> A.Region -> Can.OverloadName -> Can.Type -> Map.Map Name.Name Variable -> Variable -> IO Outcome
unifyClause rank pools region (_, name) clauseType vars provider =
  do  clause <- typeToVariable rank pools =<< Instantiate.fromSrcType (Map.map VarN vars) clauseType
      answer <- Unify.unify clause provider
      case answer of
        Unify.Ok newVars ->
          do  introduce rank pools newVars
              return Settled

        Unify.Err newVars actualType expectedType ->
          do  introduce rank pools newVars
              return $ Clash $
                Error.BadExpr region (Error.Foreign name) actualType (Error.NoExpectation expectedType)


-- Check that a variable has rank == noRank, meaning that it can be generalized.
isGeneric :: Variable -> IO ()
isGeneric var =
  do  (Descriptor _ rank _ _) <- UF.get var
      if rank == noRank
        then return ()
        else
          do  tipe <- Type.toErrorType var
              error $
                "You ran into a compiler bug. Here are some details for the developers:\n\n"
                ++ "    " ++ show (ET.toDoc L.empty RT.None tipe) ++ " [rank = " ++ show rank ++ "]\n\n"
                ++
                  "Please create an <http://sscce.org/> and then report it\n\
                  \at <https://github.com/elm/compiler/issues>\n\n"



-- EXPECTATIONS TO VARIABLE


expectedToVariable :: Int -> Pools -> Error.Expected Type -> IO Variable
expectedToVariable rank pools expectation =
  typeToVariable rank pools $
    case expectation of
      Error.NoExpectation tipe ->
        tipe

      Error.FromContext _ _ tipe ->
        tipe

      Error.FromAnnotation _ _ _ tipe ->
        tipe


patternExpectationToVariable :: Int -> Pools -> Error.PExpected Type -> IO Variable
patternExpectationToVariable rank pools expectation =
  typeToVariable rank pools $
    case expectation of
      Error.PNoExpectation tipe ->
        tipe

      Error.PFromContext _ _ tipe ->
        tipe



-- ERROR HELPERS


addError :: State -> Error.Error -> State
addError state err =
  state { _errors = err : _errors state }



-- OCCURS CHECK


occurs :: State -> (Name.Name, A.Located Variable) -> IO State
occurs state (name, A.At region variable) =
  do  hasOccurred <- Occurs.occurs variable
      if hasOccurred
        then
          do  errorType <- Type.toErrorType variable
              (Descriptor _ rank mark copy) <- UF.get variable
              UF.set variable (Descriptor Error rank mark copy)
              return $ addError state (Error.InfiniteType region name errorType)
        else
          return state



-- GENERALIZE


{-| Every variable has rank less than or equal to the maxRank of the pool.
This sorts variables into the young and old pools accordingly.
-}
generalize :: Mark -> Mark -> Int -> Pools -> IO ()
generalize youngMark visitMark youngRank pools =
  do  youngVars <- MVector.read pools youngRank
      rankTable <- poolToRankTable youngMark youngRank youngVars

      -- get the ranks right for each entry.
      -- start at low ranks so that we only have to pass
      -- over the information once.
      Vector.imapM_
        (\rank table -> mapM_ (adjustRank youngMark visitMark rank) table)
        rankTable

      -- For variables that have rank lowerer than youngRank, register them in
      -- the appropriate old pool if they are not redundant.
      Vector.forM_ (Vector.unsafeInit rankTable) $ \vars ->
        forM_ vars $ \var ->
          do  isRedundant <- UF.redundant var
              if isRedundant
                then return ()
                else
                  do  (Descriptor _ rank _ _) <- UF.get var
                      MVector.modify pools (var:) rank

      -- For variables with rank youngRank
      --   If rank < youngRank: register in oldPool
      --   otherwise generalize
      forM_ (Vector.unsafeLast rankTable) $ \var ->
        do  isRedundant <- UF.redundant var
            if isRedundant
              then return ()
              else
                do  (Descriptor content rank mark copy) <- UF.get var
                    if rank < youngRank
                      then MVector.modify pools (var:) rank
                      else UF.set var $ Descriptor content noRank mark copy


poolToRankTable :: Mark -> Int -> [Variable] -> IO (Vector.Vector [Variable])
poolToRankTable youngMark youngRank youngInhabitants =
  do  mutableTable <- MVector.replicate (youngRank + 1) []

      -- Sort the youngPool variables into buckets by rank.
      forM_ youngInhabitants $ \var ->
        do  (Descriptor content rank _ copy) <- UF.get var
            UF.set var (Descriptor content rank youngMark copy)
            MVector.modify mutableTable (var:) rank

      Vector.unsafeFreeze mutableTable



-- ADJUST RANK

--
-- Adjust variable ranks such that ranks never increase as you move deeper.
-- This way the outermost rank is representative of the entire structure.
--
adjustRank :: Mark -> Mark -> Int -> Variable -> IO Int
adjustRank youngMark visitMark groupRank var =
  do  (Descriptor content rank mark copy) <- UF.get var
      if mark == youngMark then
          do  -- Set the variable as marked first because it may be cyclic.
              UF.set var $ Descriptor content rank visitMark copy
              maxRank <- adjustRankContent youngMark visitMark groupRank content
              UF.set var $ Descriptor content maxRank visitMark copy
              return maxRank

        else if mark == visitMark then
          return rank

        else
          do  let minRank = min groupRank rank
              -- TODO how can minRank ever be groupRank?
              UF.set var $ Descriptor content minRank visitMark copy
              return minRank


adjustRankContent :: Mark -> Mark -> Int -> Content -> IO Int
adjustRankContent youngMark visitMark groupRank content =
  let
    go = adjustRank youngMark visitMark groupRank
  in
    case content of
      FlexVar _ ->
          return groupRank

      FlexSuper _ _ ->
          return groupRank

      RigidVar _ ->
          return groupRank

      RigidSuper _ _ ->
          return groupRank

      Structure flatType ->
        case flatType of
          App1 _ _ args ->
            foldM (\rank arg -> max rank <$> go arg) outermostRank args

          Fun1 arg result ->
              max <$> go arg <*> go result

          EmptyRecord1 ->
              -- THEORY: an empty record never needs to get generalized
              return outermostRank

          Record1 fields extension ->
              do  extRank <- go extension
                  foldM (\rank field -> max rank <$> go field) extRank fields

          Unit1 ->
              -- THEORY: a unit never needs to get generalized
              return outermostRank

          Tuple1 a b maybeC ->
              do  ma <- go a
                  mb <- go b
                  case maybeC of
                    Nothing ->
                      return (max ma mb)

                    Just c ->
                      max (max ma mb) <$> go c

          EmptyTagRow1 ->
              -- THEORY: an empty tag row never needs to get generalized
              return outermostRank

          TagRow1 tags extension ->
              do  extRank <- go extension
                  foldM
                    (\rank args -> foldM (\r arg -> max r <$> go arg) rank args)
                    extRank
                    tags

      Alias _ _ args _ ->
          -- THEORY: anything in the realVar would be outermostRank
          foldM (\rank (_, argVar) -> max rank <$> go argVar) outermostRank args

      Error ->
          return groupRank



-- REGISTER VARIABLES


introduce :: Int -> Pools -> [Variable] -> IO ()
introduce rank pools variables =
  do  MVector.modify pools (variables++) rank
      forM_ variables $ \var ->
        UF.modify var $ \(Descriptor content _ mark copy) ->
          Descriptor content rank mark copy



-- TYPE TO VARIABLE


typeToVariable :: Int -> Pools -> Type -> IO Variable
typeToVariable rank pools tipe =
  typeToVar rank pools Map.empty tipe


-- PERF working with @mgriffith we noticed that a 784 line entry in a `let` was
-- causing a ~1.5 second slowdown. Moving it to the top-level to be a function
-- saved all that time. The slowdown seems to manifest in `typeToVar` and in
-- `register` in particular. Have not explored further yet. Top-level definitions
-- are recommended in cases like this anyway, so there is at least a safety
-- valve for now.
--
typeToVar :: Int -> Pools -> Map.Map Name.Name Variable -> Type -> IO Variable
typeToVar rank pools aliasDict tipe =
  let go = typeToVar rank pools aliasDict in
  case tipe of
    VarN v ->
      return v

    AppN home name args ->
      do  argVars <- traverse go args
          register rank pools (Structure (App1 home name argVars))

    FunN a b ->
      do  aVar <- go a
          bVar <- go b
          register rank pools (Structure (Fun1 aVar bVar))

    AliasN home name args aliasType ->
      do  argVars <- traverse (traverse go) args
          aliasVar <- typeToVar rank pools (Map.fromList argVars) aliasType
          register rank pools (Alias home name argVars aliasVar)

    PlaceHolder name ->
      return (aliasDict ! name)

    RecordN fields ext ->
      do  fieldVars <- traverse go fields
          extVar <- go ext
          register rank pools (Structure (Record1 fieldVars extVar))

    EmptyRecordN ->
      register rank pools emptyRecord1

    TagRowN tags ext ->
      do  tagVars <- traverse (traverse go) tags
          extVar <- go ext
          register rank pools (Structure (TagRow1 tagVars extVar))

    EmptyTagRowN ->
      register rank pools emptyTagRow1

    UnitN ->
      register rank pools unit1

    TupleN a b c ->
      do  aVar <- go a
          bVar <- go b
          cVar <- traverse go c
          register rank pools (Structure (Tuple1 aVar bVar cVar))


register :: Int -> Pools -> Content -> IO Variable
register rank pools content =
  do  var <- UF.fresh (Descriptor content rank noMark Nothing)
      MVector.modify pools (var:) rank
      return var


{-# NOINLINE emptyRecord1 #-}
emptyRecord1 :: Content
emptyRecord1 =
  Structure EmptyRecord1


{-# NOINLINE emptyTagRow1 #-}
emptyTagRow1 :: Content
emptyTagRow1 =
  Structure EmptyTagRow1


{-# NOINLINE unit1 #-}
unit1 :: Content
unit1 =
  Structure Unit1



-- SOURCE TYPE TO VARIABLE


srcTypeToVariable :: Int -> Pools -> Map.Map Name.Name () -> Can.Type -> IO Variable
srcTypeToVariable rank pools freeVars srcType =
  let
    nameToContent name
      | Name.isNumberType     name = FlexSuper Number (Just name)
      | Name.isComparableType name = FlexSuper Comparable (Just name)
      | Name.isAppendableType name = FlexSuper Appendable (Just name)
      | Name.isCompappendType name = FlexSuper CompAppend (Just name)
      | otherwise                  = FlexVar (Just name)

    makeVar name _ =
      UF.fresh (Descriptor (nameToContent name) rank noMark Nothing)
  in
  do  flexVars <- Map.traverseWithKey makeVar freeVars
      MVector.modify pools (Map.elems flexVars ++) rank
      srcTypeToVar rank pools flexVars srcType


srcTypeToVar :: Int -> Pools -> Map.Map Name.Name Variable -> Can.Type -> IO Variable
srcTypeToVar rank pools flexVars srcType =
  let go = srcTypeToVar rank pools flexVars in
  case srcType of
    Can.TLambda argument result ->
      do  argVar <- go argument
          resultVar <- go result
          register rank pools (Structure (Fun1 argVar resultVar))

    Can.TVar name ->
      return (flexVars ! name)

    Can.TType home name args ->
      do  argVars <- traverse go args
          register rank pools (Structure (App1 home name argVars))

    Can.TRecord fields maybeExt ->
      do  fieldVars <- traverse (srcFieldTypeToVar rank pools flexVars) fields
          extVar <-
            case maybeExt of
              Nothing -> register rank pools emptyRecord1
              Just ext -> return (flexVars ! ext)
          register rank pools (Structure (Record1 fieldVars extVar))

    Can.TTagRow tags maybeExt ->
      do  tagVars <- traverse (traverse go) tags
          extVar <-
            case maybeExt of
              Nothing -> register rank pools emptyTagRow1
              Just ext -> return (flexVars ! ext)
          register rank pools (Structure (TagRow1 tagVars extVar))

    Can.TUnit ->
      register rank pools unit1

    Can.TTuple a b c ->
      do  aVar <- go a
          bVar <- go b
          cVar <- traverse go c
          register rank pools (Structure (Tuple1 aVar bVar cVar))

    Can.TAlias home name args aliasType ->
      do  argVars <- traverse (traverse go) args
          aliasVar <-
            case aliasType of
              Can.Holey tipe ->
                srcTypeToVar rank pools (Map.fromList argVars) tipe

              Can.Filled tipe ->
                go tipe

          register rank pools (Alias home name argVars aliasVar)


srcFieldTypeToVar :: Int -> Pools -> Map.Map Name.Name Variable -> Can.FieldType -> IO Variable
srcFieldTypeToVar rank pools flexVars (Can.FieldType _ srcTipe) =
  srcTypeToVar rank pools flexVars srcTipe



-- COPY


makeCopy :: Int -> Pools -> Variable -> IO Variable
makeCopy rank pools var =
  do  copy <- makeCopyHelp rank pools var
      restore var
      return copy


makeCopyHelp :: Int -> Pools -> Variable -> IO Variable
makeCopyHelp maxRank pools variable =
  do  (Descriptor content rank _ maybeCopy) <- UF.get variable

      case maybeCopy of
        Just copy ->
          return copy

        Nothing ->
          if rank /= noRank then
            return variable

          else
            do  let makeDescriptor c = Descriptor c maxRank noMark Nothing
                copy <- UF.fresh $ makeDescriptor content
                MVector.modify pools (copy:) maxRank

                -- Link the original variable to the new variable. This lets us
                -- avoid making multiple copies of the variable we are instantiating.
                --
                -- Need to do this before recursively copying to avoid looping.
                UF.set variable $
                  Descriptor content rank noMark (Just copy)

                -- Now we recursively copy the content of the variable.
                -- We have already marked the variable as copied, so we
                -- will not repeat this work or crawl this variable again.
                case content of
                  Structure term ->
                    do  newTerm <- traverseFlatType (makeCopyHelp maxRank pools) term
                        UF.set copy $ makeDescriptor (Structure newTerm)
                        return copy

                  FlexVar _ ->
                    return copy

                  FlexSuper _ _ ->
                    return copy

                  RigidVar name ->
                    do  UF.set copy $ makeDescriptor $ FlexVar (Just name)
                        return copy

                  RigidSuper super name ->
                    do  UF.set copy $ makeDescriptor $ FlexSuper super (Just name)
                        return copy

                  Alias home name args realType ->
                    do  newArgs <- mapM (traverse (makeCopyHelp maxRank pools)) args
                        newRealType <- makeCopyHelp maxRank pools realType
                        UF.set copy $ makeDescriptor (Alias home name newArgs newRealType)
                        return copy

                  Error ->
                    return copy



-- RESTORE


restore :: Variable -> IO ()
restore variable =
  do  (Descriptor content _ _ maybeCopy) <- UF.get variable
      case maybeCopy of
        Nothing ->
          return ()

        Just _ ->
          do  UF.set variable $ Descriptor content noRank noMark Nothing
              restoreContent content


restoreContent :: Content -> IO ()
restoreContent content =
  case content of
    FlexVar _ ->
      return ()

    FlexSuper _ _ ->
      return ()

    RigidVar _ ->
      return ()

    RigidSuper _ _ ->
      return ()

    Structure term ->
      case term of
        App1 _ _ args ->
          mapM_ restore args

        Fun1 arg result ->
          do  restore arg
              restore result

        EmptyRecord1 ->
          return ()

        Record1 fields ext ->
          do  mapM_ restore fields
              restore ext

        Unit1 ->
          return ()

        Tuple1 a b maybeC ->
          do  restore a
              restore b
              case maybeC of
                Nothing -> return ()
                Just c  -> restore c

        EmptyTagRow1 ->
          return ()

        TagRow1 tags ext ->
          do  mapM_ (mapM_ restore) tags
              restore ext

    Alias _ _ args var ->
      do  mapM_ (traverse restore) args
          restore var

    Error ->
        return ()



-- TRAVERSE FLAT TYPE


traverseFlatType :: (Variable -> IO Variable) -> FlatType -> IO FlatType
traverseFlatType f flatType =
  case flatType of
    App1 home name args ->
        liftM (App1 home name) (traverse f args)

    Fun1 a b ->
        liftM2 Fun1 (f a) (f b)

    EmptyRecord1 ->
        pure EmptyRecord1

    Record1 fields ext ->
        liftM2 Record1 (traverse f fields) (f ext)

    Unit1 ->
        pure Unit1

    Tuple1 a b cs ->
        liftM3 Tuple1 (f a) (f b) (traverse f cs)

    EmptyTagRow1 ->
        pure EmptyTagRow1

    TagRow1 tags ext ->
        liftM2 TagRow1 (traverse (traverse f) tags) (f ext)
