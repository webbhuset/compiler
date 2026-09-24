{-# LANGUAGE OverloadedStrings #-}
module Optimize.Expression
  ( optimize
  , destructArgs
  , optimizePotentialTailCall
  )
  where


import Prelude hiding (cycle)
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Utils.Css as Css
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Optimize.Case as Case
import qualified Optimize.Names as Names
import qualified Type.Overload as Overload
import qualified Reporting.Annotation as A



-- OPTIMIZE


type Cycle =
  Set.Set Name.Name


-- WHAT AN OVERLOAD RESOLVED TO
--
-- A definition, which may itself need overloads passed to it, or one of the
-- enclosing definition's own `where` parameters.


fromTarget :: Overload.Target -> Names.Tracker Opt.Expr
fromTarget target =
  case target of
    Overload.Parameter name ->
      pure (Opt.VarLocal name)

    Overload.Definition (home, name) [] ->
      Names.registerGlobal home name

    Overload.Definition (home, name) inner ->
      Opt.Call
        <$> Names.registerGlobal home name
        <*> traverse fromTarget inner


optimize :: Cycle -> Can.Expr -> Names.Tracker Opt.Expr
optimize cycle (A.At region expression) =
  case expression of
    Can.VarLocal name ->
      pure (Opt.VarLocal name)

    Can.VarTopLevel home name ->
      if Set.member name cycle then
        pure (Opt.VarCycle home name)
      else
        Names.registerGlobal home name

    Can.VarKernel home name ->
      Names.registerKernel home (Opt.VarKernel home name)

    Can.VarForeign home name _ ->
      Names.registerGlobal home name

    -- Resolved by Type.Overload once the use site's type was known.
    Can.VarOverload (Can.Dispatch useHome useRegion _) (ovHome, ovName) _ ->
      case Overload.lookupResolved useHome useRegion of
        target : _ ->
          fromTarget target

        [] ->
          Names.registerGlobal ovHome ovName

    -- A value that asks for overloads takes them as leading arguments, added
    -- here so that the type it is written with stays the type it has.
    Can.VarConstrained (Can.Dispatch useHome useRegion _) home name _ _ ->
      case Overload.lookupResolved useHome useRegion of
        [] ->
          Names.registerGlobal home name

        targets ->
          Opt.Call
            <$> Names.registerGlobal home name
            <*> traverse fromTarget targets

    Can.VarCtor opts home name index _ ->
      Names.registerCtor home name index opts

    Can.VarTag home name _ ->
      Names.registerGlobal home name

    Can.VarDebug home name _ ->
      Names.registerDebug name home region

    Can.Widen inner ->
      -- a coercion: the runtime value is untouched
      optimize cycle inner

    Can.VarOperator _ home name _ ->
      Names.registerGlobal home name

    Can.Chr chr ->
      Names.registerKernel Name.utils (Opt.Chr chr)

    Can.Str str ->
      pure (Opt.Str str)

    Can.Int int ->
      pure (Opt.Int int)

    Can.Float float ->
      pure (Opt.Float float)

    Can.List entries ->
      Names.registerKernel Name.list Opt.List
        <*> traverse (optimize cycle) entries

    Can.Negate expr ->
      do  func <- Names.registerGlobal ModuleName.basics Name.negate
          arg <- optimize cycle expr
          pure $ Opt.Call func [arg]

    Can.Binop _ home name _ left right ->
      do  optFunc <- Names.registerGlobal home name
          optLeft <- optimize cycle left
          optRight <- optimize cycle right
          return (Opt.Call optFunc [optLeft, optRight])

    Can.Lambda args body ->
      do  (argNames, destructors) <- destructArgs args
          obody <- optimize cycle body
          pure $ Opt.Function argNames (foldr Opt.Destruct obody destructors)

    -- Kept as one call so that a constrained function is applied to its
    -- overloads and its own arguments in a single step.
    Can.Call func@(A.At _ (Can.VarConstrained (Can.Dispatch useHome useRegion _) home name _ _)) args ->
      Opt.Call
        <$> Names.registerGlobal home name
        <*> ((++)
              <$> traverse fromTarget (Overload.lookupResolved useHome useRegion)
              <*> optimizeArgs cycle func args)

    Can.Call func args ->
      Opt.Call
        <$> optimize cycle func
        <*> optimizeArgs cycle func args

    Can.If branches finally ->
      let
        optimizeBranch (condition, branch) =
          (,)
            <$> optimize cycle condition
            <*> optimize cycle branch
      in
      Opt.If
        <$> traverse optimizeBranch branches
        <*> optimize cycle finally

    Can.Let def body ->
      optimizeDef cycle def =<< optimize cycle body

    Can.LetRec defs body ->
      case defs of
        [def] ->
          Opt.Let
            <$> optimizePotentialTailCallDef cycle def
            <*> optimize cycle body

        _ ->
          do  obody <- optimize cycle body
              foldM (\bod def -> optimizeDef cycle def bod) obody defs

    Can.LetDestruct pattern expr body ->
      do  (name, destructs) <- destruct pattern
          oexpr <- optimize cycle expr
          obody <- optimize cycle body
          pure $
            Opt.Let (Opt.Def name oexpr) (foldr Opt.Destruct obody destructs)

    Can.Case expr branches ->
      let
        optimizeBranch root (Can.CaseBranch pattern branch) =
          do  destructors <- destructCase root pattern
              obranch <- optimize cycle branch
              pure (pattern, foldr Opt.Destruct obranch destructors)
      in
      do  temp <- Names.generate
          oexpr <- optimize cycle expr
          case oexpr of
            Opt.VarLocal root ->
              Case.optimize temp root <$> traverse (optimizeBranch root) branches

            _ ->
              do  obranches <- traverse (optimizeBranch temp) branches
                  return $ Opt.Let (Opt.Def temp oexpr) (Case.optimize temp temp obranches)

    Can.Accessor field ->
      Names.registerField field (Opt.Accessor field)

    Can.Access record (A.At _ field) ->
      do  optRecord <- optimize cycle record
          Names.registerField field (Opt.Access optRecord field)

    Can.Update _ record updates ->
      Names.registerFieldDict updates Opt.Update
        <*> optimize cycle record
        <*> traverse (optimizeUpdate cycle) updates

    Can.Record fields ->
      Names.registerFieldDict fields Opt.Record
        <*> traverse (optimize cycle) fields

    Can.Unit ->
      Names.registerKernel Name.utils Opt.Unit

    Can.Tuple a b maybeC ->
      Names.registerKernel Name.utils Opt.Tuple
        <*> optimize cycle a
        <*> optimize cycle b
        <*> traverse (optimize cycle) maybeC

    Can.Shader src (Shader.Types attributes uniforms _varyings) ->
      pure (Opt.Shader src (Map.keysSet attributes) (Map.keysSet uniforms))

    Can.Css home content@(Css.Content _ (Css.Types classes keyframes vars)) ->
      Names.registerFieldList
        (Set.toList classes ++ Set.toList keyframes ++ Map.keys vars)
        (Opt.Css home content)



-- OPTIMIZE ARGS
--
-- The first argument of Worker.spawn is a direct reference to a top-level
-- worker program (Nitpick.Workers guarantees the shape). It compiles to a
-- WorkerRef, which becomes the URL of a separately generated bundle, and
-- deliberately does NOT register the referenced global as a dependency:
-- the worker's code must not end up in the spawning bundle.


optimizeArgs :: Cycle -> Can.Expr -> [Can.Expr] -> Names.Tracker [Opt.Expr]
optimizeArgs cycle func args =
  case (A.toValue func, args) of
    (Can.VarForeign home name _, A.At _ (Can.VarForeign progHome progName _) : rest)
      | isWorkerSpawn home name ->
          (:) (Opt.WorkerRef (Opt.Global progHome progName))
            <$> traverse (optimize cycle) rest

    (Can.VarForeign home name _, A.At _ (Can.VarTopLevel progHome progName) : rest)
      | isWorkerSpawn home name ->
          (:) (Opt.WorkerRef (Opt.Global progHome progName))
            <$> traverse (optimize cycle) rest

    _ ->
      traverse (optimize cycle) args


isWorkerSpawn :: ModuleName.Canonical -> Name.Name -> Bool
isWorkerSpawn home name =
  home == ModuleName.workers && name == Name.fromChars "spawn"



-- UPDATE


optimizeUpdate :: Cycle -> Can.FieldUpdate -> Names.Tracker Opt.Expr
optimizeUpdate cycle (Can.FieldUpdate _ expr) =
  optimize cycle expr



-- DEFINITION


optimizeDef :: Cycle -> Can.Def -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDef cycle def body =
  case def of
    Can.Def (A.At _ name) args expr ->
      optimizeDefHelp cycle name args expr body

    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      optimizeDefHelp cycle name (map fst typedArgs) expr body


optimizeDefHelp :: Cycle -> Name.Name -> [Can.Pattern] -> Can.Expr -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDefHelp cycle name args expr body =
  do  oexpr <- optimize cycle expr
      case args of
        [] ->
          pure $ Opt.Let (Opt.Def name oexpr) body

        _ ->
          do  (argNames, destructors) <- destructArgs args
              let ofunc = Opt.Function argNames (foldr Opt.Destruct oexpr destructors)
              pure $ Opt.Let (Opt.Def name ofunc) body



-- DESTRUCTURING


destructArgs :: [Can.Pattern] -> Names.Tracker ([Name.Name], [Opt.Destructor])
destructArgs args =
  do  (argNames, destructorLists) <- unzip <$> traverse destruct args
      return (argNames, concat destructorLists)


destructCase :: Name.Name -> Can.Pattern -> Names.Tracker [Opt.Destructor]
destructCase rootName pattern =
  reverse <$> destructHelp (Opt.Root rootName) pattern []


destruct :: Can.Pattern -> Names.Tracker (Name.Name, [Opt.Destructor])
destruct pattern@(A.At _ ptrn) =
  case ptrn of
    Can.PVar name ->
      pure (name, [])

    Can.PAlias subPattern name ->
      do  revDs <- destructHelp (Opt.Root name) subPattern []
          pure (name, reverse revDs)

    _ ->
      do  name <- Names.generate
          revDs <- destructHelp (Opt.Root name) pattern []
          pure (name, reverse revDs)


destructHelp :: Opt.Path -> Can.Pattern -> [Opt.Destructor] -> Names.Tracker [Opt.Destructor]
destructHelp path (A.At region pattern) revDs =
  case pattern of
    Can.PAnything ->
      pure revDs

    Can.PVar name ->
      pure (Opt.Destructor name path : revDs)

    Can.PRecord fields ->
      let
        toDestruct name =
          Opt.Destructor name (Opt.Field name path)
      in
      Names.registerFieldList fields (map toDestruct fields ++ revDs)

    Can.PAlias subPattern name ->
      destructHelp (Opt.Root name) subPattern $
        Opt.Destructor name path : revDs

    Can.PUnit ->
      pure revDs

    Can.PTuple a b Nothing ->
      destructTwo path a b revDs

    Can.PTuple a b (Just c) ->
      case path of
        Opt.Root _ ->
          destructHelp (Opt.Index Index.third path) c =<<
            destructHelp (Opt.Index Index.second path) b =<<
              destructHelp (Opt.Index Index.first path) a revDs

        _ ->
          do  name <- Names.generate
              let newRoot = Opt.Root name
              destructHelp (Opt.Index Index.third newRoot) c =<<
                destructHelp (Opt.Index Index.second newRoot) b =<<
                  destructHelp (Opt.Index Index.first newRoot) a (Opt.Destructor name path : revDs)

    Can.PList [] ->
      pure revDs

    Can.PList (hd:tl) ->
      destructTwo path hd (A.At region (Can.PList tl)) revDs

    Can.PCons hd tl ->
      destructTwo path hd tl revDs

    Can.PChr _ ->
      pure revDs

    Can.PStr _ ->
      pure revDs

    Can.PInt _ ->
      pure revDs

    Can.PBool _ _ ->
      pure revDs

    Can.PCtor _ _ (Can.Union _ _ _ opts) _ _ args ->
      case args of
        [Can.PatternCtorArg _ _ arg] ->
          case opts of
            Can.Normal -> destructHelp (Opt.Index Index.first path) arg revDs
            Can.Unbox  -> destructHelp (Opt.Unbox path) arg revDs
            Can.Enum   -> destructHelp (Opt.Index Index.first path) arg revDs

        _ ->
          case path of
            Opt.Root _ ->
              foldM (destructCtorArg path) revDs args

            _ ->
              do  name <- Names.generate
                  foldM (destructCtorArg (Opt.Root name)) (Opt.Destructor name path : revDs) args

    Can.PTag _ _ _ args ->
      case args of
        [arg] ->
          destructHelp (Opt.Index Index.first path) arg revDs

        _ ->
          case path of
            Opt.Root _ ->
              foldM destructTagArg revDs (Index.indexedMap (,) args)

            _ ->
              do  name <- Names.generate
                  foldM
                    (\ds ia -> destructTagArgAt (Opt.Root name) ds ia)
                    (Opt.Destructor name path : revDs)
                    (Index.indexedMap (,) args)
      where
        destructTagArg revDs_ (index, arg) =
          destructHelp (Opt.Index index path) arg revDs_

        destructTagArgAt root revDs_ (index, arg) =
          destructHelp (Opt.Index index root) arg revDs_


destructTwo :: Opt.Path -> Can.Pattern -> Can.Pattern -> [Opt.Destructor] -> Names.Tracker [Opt.Destructor]
destructTwo path a b revDs =
  case path of
    Opt.Root _ ->
      destructHelp (Opt.Index Index.second path) b =<<
        destructHelp (Opt.Index Index.first path) a revDs

    _ ->
      do  name <- Names.generate
          let newRoot = Opt.Root name
          destructHelp (Opt.Index Index.second newRoot) b =<<
            destructHelp (Opt.Index Index.first newRoot) a (Opt.Destructor name path : revDs)


destructCtorArg :: Opt.Path -> [Opt.Destructor] -> Can.PatternCtorArg -> Names.Tracker [Opt.Destructor]
destructCtorArg path revDs (Can.PatternCtorArg index _ arg) =
  destructHelp (Opt.Index index path) arg revDs



-- TAIL CALL


optimizePotentialTailCallDef :: Cycle -> Can.Def -> Names.Tracker Opt.Def
optimizePotentialTailCallDef cycle def =
  case def of
    Can.Def (A.At _ name) args expr ->
      optimizePotentialTailCall cycle name args expr

    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      optimizePotentialTailCall cycle name (map fst typedArgs) expr


optimizePotentialTailCall :: Cycle -> Name.Name -> [Can.Pattern] -> Can.Expr -> Names.Tracker Opt.Def
optimizePotentialTailCall cycle name args expr =
  do  (argNames, destructors) <- destructArgs args
      toTailDef name argNames destructors <$>
        optimizeTail cycle name argNames (tailHole name (length argNames) expr) expr


-- The overloads were optimized once already, so the callee is taken bare here
-- rather than through optimize, which would add them a second time.
callWithDicts :: Cycle -> Can.Expr -> [Opt.Expr] -> [Opt.Expr] -> Names.Tracker Opt.Expr
callWithDicts cycle func odicts oargs =
  case A.toValue func of
    Can.VarConstrained _ home name _ _ ->
      do  ofunc <- Names.registerGlobal home name
          pure $ Opt.Call ofunc oargs

    _ ->
      do  ofunc <- optimize cycle func
          pure $ Opt.Call ofunc (drop (length odicts) oargs)


-- The hole, when there is one, is the constructor field that a tail call
-- modulo cons sits in; see TAIL CALL MODULO CONS below.
optimizeTail :: Cycle -> Name.Name -> [Name.Name] -> Maybe Index.ZeroBased -> Can.Expr -> Names.Tracker Opt.Expr
optimizeTail cycle rootName argNames hole locExpr@(A.At _ expression) =
  case expression of
    Can.Binop _ home name _ left right
      | hole == Just Index.second && isCons home name && isSelfCall rootName (length argNames) right ->
          optimizeTailBuild cycle rootName argNames hole (Names.registerGlobal home name) [left, right]

    Can.Call (A.At _ (Can.VarCtor Can.Normal home name index annotation)) args
      | Maybe.isJust hole && hole == selfArgument rootName (length argNames) annotation args ->
          optimizeTailBuild cycle rootName argNames hole (Names.registerCtor home name index Can.Normal) args

    Can.Call func args ->
      either id (Opt.TailCall rootName) <$> optimizeSelfCall cycle rootName argNames func args

    Can.If branches finally ->
      let
        optimizeBranch (condition, branch) =
          (,)
            <$> optimize cycle condition
            <*> optimizeTail cycle rootName argNames hole branch
      in
      Opt.If
        <$> traverse optimizeBranch branches
        <*> optimizeTail cycle rootName argNames hole finally

    Can.Let def body ->
      optimizeDef cycle def =<< optimizeTail cycle rootName argNames hole body

    Can.LetRec defs body ->
      case defs of
        [def] ->
          Opt.Let
            <$> optimizePotentialTailCallDef cycle def
            <*> optimizeTail cycle rootName argNames hole body

        _ ->
          do  obody <- optimizeTail cycle rootName argNames hole body
              foldM (\bod def -> optimizeDef cycle def bod) obody defs

    Can.LetDestruct pattern expr body ->
      do  (dname, destructors) <- destruct pattern
          oexpr <- optimize cycle expr
          obody <- optimizeTail cycle rootName argNames hole body
          pure $
            Opt.Let (Opt.Def dname oexpr) (foldr Opt.Destruct obody destructors)

    Can.Case expr branches ->
      let
        optimizeBranch root (Can.CaseBranch pattern branch) =
          do  destructors <- destructCase root pattern
              obranch <- optimizeTail cycle rootName argNames hole branch
              pure (pattern, foldr Opt.Destruct obranch destructors)
      in
      do  temp <- Names.generate
          oexpr <- optimize cycle expr
          case oexpr of
            Opt.VarLocal root ->
              Case.optimize temp root <$> traverse (optimizeBranch root) branches

            _ ->
              do  obranches <- traverse (optimizeBranch temp) branches
                  return $ Opt.Let (Opt.Def temp oexpr) (Case.optimize temp temp obranches)

    _ ->
      optimize cycle locExpr



-- Either the call as usual, or, for a call to the function itself with
-- exactly its parameters, the pairs a loop needs to reassign them.
optimizeSelfCall :: Cycle -> Name.Name -> [Name.Name] -> Can.Expr -> [Can.Expr] -> Names.Tracker (Either Opt.Expr [(Name.Name, Opt.Expr)])
optimizeSelfCall cycle rootName argNames func args =
  do  -- A constrained function passes its own overloads along, so a self
      -- call still lines up with the parameter list and stays a tail call.
      odicts <-
        case A.toValue func of
          Can.VarConstrained (Can.Dispatch useHome useRegion _) _ _ _ _ ->
            traverse fromTarget (Overload.lookupResolved useHome useRegion)

          _ ->
            pure []

      oargs <- (odicts ++) <$> optimizeArgs cycle func args

      if isSelfName rootName func
        then
          case Index.indexedZipWith (\_ a b -> (a,b)) argNames oargs of
            Index.LengthMatch pairs ->
              pure (Right pairs)

            Index.LengthMismatch _ _ ->
              Left <$> callWithDicts cycle func odicts oargs
        else
          Left <$> callWithDicts cycle func odicts oargs


isSelfName :: Name.Name -> Can.Expr -> Bool
isSelfName rootName (A.At _ func) =
  case func of
    Can.VarLocal      name          -> rootName == name
    Can.VarTopLevel _ name          -> rootName == name
    Can.VarConstrained _ _ name _ _ -> rootName == name
    _                               -> False



-- TAIL CALL MODULO CONS
--
-- `x :: recurse ...` in tail position is not a tail call, but the only
-- thing left to do after the call is to put its result in one field of a
-- freshly built cell. So the loop can build that cell up front with the
-- field left empty, hang it on the cell built by the previous iteration,
-- and carry on; the base case then fills the last field and returns what
-- hangs off a sentinel. The same holds for any saturated constructor with
-- the self call as exactly one of its arguments.
--
-- The field written into has to be the same at every site, since the
-- previous iteration's empty field is what the next one fills. A function
-- whose sites disagree gets no loop for them, and any plain self tail
-- call in the same function keeps working as before: it appends nothing.


-- Whether the sites agree on one field, and which.
tailHole :: Name.Name -> Int -> Can.Expr -> Maybe Index.ZeroBased
tailHole rootName arity expr =
  case Set.toList (tailHoles rootName arity expr) of
    [hole] -> Just (indexFromInt hole)
    _      -> Nothing


tailHoles :: Name.Name -> Int -> Can.Expr -> Set.Set Int
tailHoles rootName arity (A.At _ expression) =
  case expression of
    Can.Binop _ home name _ _ right
      | isCons home name && isSelfCall rootName arity right ->
          Set.singleton 1

    Can.Call (A.At _ (Can.VarCtor Can.Normal _ _ _ annotation)) args ->
      maybe Set.empty (Set.singleton . Index.toMachine) (selfArgument rootName arity annotation args)

    Can.If branches finally ->
      Set.unions (tailHoles rootName arity finally : map (tailHoles rootName arity . snd) branches)

    Can.Let _ body ->
      tailHoles rootName arity body

    Can.LetRec _ body ->
      tailHoles rootName arity body

    Can.LetDestruct _ _ body ->
      tailHoles rootName arity body

    Can.Case _ branches ->
      Set.unions (map (\(Can.CaseBranch _ branch) -> tailHoles rootName arity branch) branches)

    _ ->
      Set.empty


-- The one argument of a saturated constructor that is a self call.
selfArgument :: Name.Name -> Int -> Can.Annotation -> [Can.Expr] -> Maybe Index.ZeroBased
selfArgument rootName arity (Can.Forall _ tipe) args =
  if ctorArity tipe /= length args then
    Nothing
  else
    case [ i | (i, arg) <- Index.indexedMap (,) args, isSelfCall rootName arity arg ] of
      [i] -> Just i
      _   -> Nothing


ctorArity :: Can.Type -> Int
ctorArity tipe =
  case tipe of
    Can.TLambda _ result -> 1 + ctorArity result
    _                    -> 0


-- A call to the function itself with exactly its parameters, the overloads
-- a constrained function passes along included.
isSelfCall :: Name.Name -> Int -> Can.Expr -> Bool
isSelfCall rootName arity (A.At _ expression) =
  case expression of
    Can.Call func args ->
      let
        dicts =
          case A.toValue func of
            Can.VarConstrained (Can.Dispatch useHome useRegion _) _ _ _ _ ->
              length (Overload.lookupResolved useHome useRegion)

            _ ->
              0
      in
      isSelfName rootName func && dicts + length args == arity

    _ ->
      False


isCons :: ModuleName.Canonical -> Name.Name -> Bool
isCons home name =
  home == ModuleName.list && name == Name.fromChars "cons"


indexFromInt :: Int -> Index.ZeroBased
indexFromInt n =
  if n <= 0 then Index.first else Index.next (indexFromInt (n - 1))


-- The constructor application with the self call's argument left as a
-- placeholder, and the pairs the loop reassigns. If the argument turns
-- out not to be a self call after all, this is an ordinary application.
optimizeTailBuild :: Cycle -> Name.Name -> [Name.Name] -> Maybe Index.ZeroBased -> Names.Tracker Opt.Expr -> [Can.Expr] -> Names.Tracker Opt.Expr
optimizeTailBuild cycle rootName argNames maybeHole registerCtor args =
  do  ctor <- registerCtor
      case (maybeHole, splitAt (maybe 0 Index.toMachine maybeHole) args) of
        (Just hole, (before, A.At _ (Can.Call func selfArgs) : after)) ->
          do  obefore <- traverse (optimize cycle) before
              oafter <- traverse (optimize cycle) after
              result <- optimizeSelfCall cycle rootName argNames func selfArgs
              placeholder <- Names.registerKernel Name.list (Opt.List [])
              pure $
                case result of
                  Right pairs ->
                    Opt.TailBuild rootName hole (Opt.Call ctor (obefore ++ placeholder : oafter)) pairs

                  Left ocall ->
                    Opt.Call ctor (obefore ++ ocall : oafter)

        _ ->
          Opt.Call ctor <$> traverse (optimize cycle) args



-- DETECT TAIL CALLS


toTailDef :: Name.Name -> [Name.Name] -> [Opt.Destructor] -> Opt.Expr -> Opt.Def
toTailDef name argNames destructors body =
  if hasTailCall body then
    Opt.TailDef name argNames (foldr Opt.Destruct body destructors)
  else
    Opt.Def name (Opt.Function argNames (foldr Opt.Destruct body destructors))


hasTailCall :: Opt.Expr -> Bool
hasTailCall expression =
  case expression of
    Opt.TailCall _ _ ->
      True

    Opt.TailBuild _ _ _ _ ->
      True

    Opt.If branches finally ->
      hasTailCall finally || any (hasTailCall . snd) branches

    Opt.Let _ body ->
      hasTailCall body

    Opt.Destruct _ body ->
      hasTailCall body

    Opt.Case _ _ decider jumps ->
      decidecHasTailCall decider || any (hasTailCall . snd) jumps

    _ ->
      False


decidecHasTailCall :: Opt.Decider Opt.Choice -> Bool
decidecHasTailCall decider =
  case decider of
    Opt.Leaf choice ->
      case choice of
        Opt.Inline expr ->
          hasTailCall expr

        Opt.Jump _ ->
          False

    Opt.Chain _ success failure ->
      decidecHasTailCall success || decidecHasTailCall failure

    Opt.FanOut _ tests fallback ->
      decidecHasTailCall fallback || any (decidecHasTailCall . snd) tests
