{-# LANGUAGE OverloadedStrings #-}
module Type.Type
  ( Constraint(..)
  , Need(..)
  , Clause(..)
  , exists
  , Variable
  , FlatType(..)
  , Type(..)
  , Descriptor(Descriptor)
  , Content(..)
  , SuperType(..)
  , noRank
  , outermostRank
  , Mark
  , noMark
  , nextMark
  , (==>)
  , int, float, char, string, bool, never
  , vec2, vec3, vec4, mat4, texture
  , mkFlexVar
  , mkFlexNumber
  , unnamedFlexVar
  , unnamedFlexSuper
  , nameToFlex
  , nameToRigid
  , toAnnotation
  , toRelatedTypes
  , toErrorType
  )
  where


import Control.Monad (foldM)
import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import Data.Word (Word32)

import qualified AST.Canonical as Can
import qualified AST.Utils.Type as Type
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import qualified Type.Error as ET
import qualified Type.UnionFind as UF



-- CONSTRAINTS


data Constraint
  = CTrue
  | CSaveTheEnvironment
  | CEqual A.Region E.Category Type (E.Expected Type)
  | CLocal A.Region Name.Name (E.Expected Type)
  | CForeign A.Region Name.Name Can.Annotation (E.Expected Type)
  | CPattern A.Region E.PCategory Type (E.PExpected Type)
  | CAnd [Constraint]
  | CLet
      { _rigidVars :: [Variable]
      , _flexVars :: [Variable]
      , _header :: Map.Map Name.Name (A.Located Type)
      , _headerCon :: Constraint
      , _bodyCon :: Constraint
      }
  | CDispatch [Need] [Clause]


-- An overloaded name used somewhere, with the solver variables standing for
-- the type it is used at. Once the dispatch variable settles, the solver looks
-- the definition up and unifies the rest of the clause with it, so that a type
-- the definition determines -- the element type of a container, say -- is
-- known before anything generalizes. See Type.Overload for the rest.
data Need =
  Need
    { _need_region :: A.Region
    , _need_name :: Can.OverloadName
    , _need_dispatch :: Variable
    , _need_type :: Can.Type                    -- the clause, or the abstract signature
    , _need_vars :: Map.Map Name.Name Variable  -- its type variables, as instantiated here
    }


-- A `where` clause of the enclosing definition, over its rigid variables.
data Clause =
  Clause
    { _clause :: Can.Constraint
    , _clause_dispatch :: Variable
    , _clause_vars :: Map.Map Name.Name Variable
    }


exists :: [Variable] -> Constraint -> Constraint
exists flexVars constraint =
  CLet [] flexVars Map.empty constraint CTrue



-- TYPE PRIMITIVES


type Variable =
    UF.Point Descriptor


data FlatType
    = App1 ModuleName.Canonical Name.Name [Variable]
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Map.Map Name.Name Variable) Variable
    | Unit1
    | Tuple1 Variable Variable (Maybe Variable)
    | EmptyTagRow1
    | TagRow1 (Map.Map Can.TagKey [Variable]) Variable


data Type
    = PlaceHolder Name.Name
    | AliasN ModuleName.Canonical Name.Name [(Name.Name, Type)] Type
    | VarN Variable
    | AppN ModuleName.Canonical Name.Name [Type]
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Map.Map Name.Name Type) Type
    | UnitN
    | TupleN Type Type (Maybe Type)
    | EmptyTagRowN
    | TagRowN (Map.Map Can.TagKey [Type]) Type



-- DESCRIPTORS


data Descriptor =
  Descriptor
    { _content :: Content
    , _rank :: Int
    , _mark :: Mark
    , _copy :: Maybe Variable
    }


data Content
    = FlexVar (Maybe Name.Name)
    | FlexSuper SuperType (Maybe Name.Name)
    | RigidVar Name.Name
    | RigidSuper SuperType Name.Name
    | Structure FlatType
    | Alias ModuleName.Canonical Name.Name [(Name.Name,Variable)] Variable
    | Error


data SuperType
  = Number
  | Comparable
  | Appendable
  | CompAppend
  deriving (Eq)


makeDescriptor :: Content -> Descriptor
makeDescriptor content =
  Descriptor content noRank noMark Nothing



-- RANKS


noRank :: Int
noRank =
  0


outermostRank :: Int
outermostRank =
  1



-- MARKS


newtype Mark = Mark Word32
  deriving (Eq, Ord)


noMark :: Mark
noMark =
  Mark 2


occursMark :: Mark
occursMark =
  Mark 1


getVarNamesMark :: Mark
getVarNamesMark =
  Mark 0


{-# INLINE nextMark #-}
nextMark :: Mark -> Mark
nextMark (Mark mark) =
  Mark (mark + 1)



-- FUNCTION TYPES


infixr 9 ==>


{-# INLINE (==>) #-}
(==>) :: Type -> Type -> Type
(==>) =
  FunN



-- PRIMITIVE TYPES


{-# NOINLINE int #-}
int :: Type
int = AppN ModuleName.basics "Int" []


{-# NOINLINE float #-}
float :: Type
float = AppN ModuleName.basics "Float" []


{-# NOINLINE char #-}
char :: Type
char = AppN ModuleName.char "Char" []


{-# NOINLINE string #-}
string :: Type
string = AppN ModuleName.string "String" []


{-# NOINLINE bool #-}
bool :: Type
bool = AppN ModuleName.basics "Bool" []


{-# NOINLINE never #-}
never :: Type
never = AppN ModuleName.basics "Never" []



-- WEBGL TYPES


{-# NOINLINE vec2 #-}
vec2 :: Type
vec2 = AppN ModuleName.vector2 "Vec2" []


{-# NOINLINE vec3 #-}
vec3 :: Type
vec3 = AppN ModuleName.vector3 "Vec3" []


{-# NOINLINE vec4 #-}
vec4 :: Type
vec4 = AppN ModuleName.vector4 "Vec4" []


{-# NOINLINE mat4 #-}
mat4 :: Type
mat4 = AppN ModuleName.matrix4 "Mat4" []


{-# NOINLINE texture #-}
texture :: Type
texture = AppN ModuleName.texture "Texture" []



-- MAKE FLEX VARIABLES


mkFlexVar :: IO Variable
mkFlexVar =
  UF.fresh flexVarDescriptor


{-# NOINLINE flexVarDescriptor #-}
flexVarDescriptor :: Descriptor
flexVarDescriptor =
  makeDescriptor unnamedFlexVar


{-# NOINLINE unnamedFlexVar #-}
unnamedFlexVar :: Content
unnamedFlexVar =
  FlexVar Nothing



-- MAKE FLEX NUMBERS


mkFlexNumber :: IO Variable
mkFlexNumber =
  UF.fresh flexNumberDescriptor


{-# NOINLINE flexNumberDescriptor #-}
flexNumberDescriptor :: Descriptor
flexNumberDescriptor =
  makeDescriptor (unnamedFlexSuper Number)


unnamedFlexSuper :: SuperType -> Content
unnamedFlexSuper super =
  FlexSuper super Nothing



-- MAKE NAMED VARIABLES


nameToFlex :: Name.Name -> IO Variable
nameToFlex name =
  UF.fresh $ makeDescriptor $
    maybe FlexVar FlexSuper (toSuper name) (Just name)


nameToRigid :: Name.Name -> IO Variable
nameToRigid name =
  UF.fresh $ makeDescriptor $
    maybe RigidVar RigidSuper (toSuper name) name


toSuper :: Name.Name -> Maybe SuperType
toSuper name =
  if Name.isNumberType name then
      Just Number

  else if Name.isComparableType name then
      Just Comparable

  else if Name.isAppendableType name then
      Just Appendable

  else if Name.isCompappendType name then
      Just CompAppend

  else
      Nothing



-- TO TYPE ANNOTATION


toAnnotation :: Variable -> IO Can.Annotation
toAnnotation variable =
  do  userNames <- getVarNames variable Map.empty
      (tipe, NameState freeVars _ _ _ _ _) <-
        State.runStateT (variableToCanType variable) (makeNameState userNames)
      return $ Can.Forall freeVars tipe


-- Render several variables under one naming. Two of them come out written
-- with the same type variable name only when they really are the same
-- variable, which is what makes it safe to compare the results.
toRelatedTypes :: [Variable] -> IO [Can.Type]
toRelatedTypes variables =
  do  userNames <- foldM (flip getVarNames) Map.empty variables
      State.evalStateT (traverse variableToCanType variables) (makeNameState userNames)


variableToCanType :: Variable -> StateT NameState IO Can.Type
variableToCanType variable =
  variableToCanTypeHelp [] variable


-- The `aliasArgs` are the arguments of the type alias whose body we are
-- currently converting. Any variable equivalent to one of them becomes a
-- `Can.TVar` with the name of the alias parameter, so the body is emitted as
-- `Can.Holey` and each argument is only converted once (in the argument list).
--
-- Converting the body as `Can.Filled` instead would re-convert every argument
-- once per level of alias nesting, which is exponential for things like
-- `type alias Parts = Part1 (Part2 (Part3 {}))` where each `PartN` is an
-- extensible record. See https://github.com/elm/compiler/issues/1897
--
variableToCanTypeHelp :: [(Variable, Can.Type)] -> Variable -> StateT NameState IO Can.Type
variableToCanTypeHelp aliasArgs variable =
  do  maybeArg <- liftIO $ findAliasArg aliasArgs variable
      case maybeArg of
        Just argType ->
          return argType

        Nothing ->
          do  (Descriptor content _ _ _) <- liftIO $ UF.get variable
              contentToCanType aliasArgs variable content


contentToCanType :: [(Variable, Can.Type)] -> Variable -> Content -> StateT NameState IO Can.Type
contentToCanType aliasArgs variable content =
  case content of
    Structure term ->
        termToCanType aliasArgs term

    FlexVar maybeName ->
      case maybeName of
        Just name ->
          return (Can.TVar name)

        Nothing ->
          do  name <- getFreshVarName
              liftIO $ UF.modify variable (\desc -> desc { _content = FlexVar (Just name) })
              return (Can.TVar name)

    FlexSuper super maybeName ->
      case maybeName of
        Just name ->
          return (Can.TVar name)

        Nothing ->
          do  name <- getFreshSuperName super
              liftIO $ UF.modify variable (\desc -> desc { _content = FlexSuper super (Just name) })
              return (Can.TVar name)

    RigidVar name ->
        return (Can.TVar name)

    RigidSuper _ name ->
        return (Can.TVar name)

    Alias home name args realVariable ->
        do  canArgs <- traverse (traverse (variableToCanTypeHelp aliasArgs)) args
            let bodyArgs = map (\(argName, argVar) -> (argVar, Can.TVar argName)) args
            canType <- variableToCanTypeHelp bodyArgs realVariable
            return (Can.TAlias home name canArgs (Can.Holey canType))

    Error ->
        error "cannot handle Error types in variableToCanType"


termToCanType :: [(Variable, Can.Type)] -> FlatType -> StateT NameState IO Can.Type
termToCanType aliasArgs term =
  let
    go = variableToCanTypeHelp aliasArgs
  in
  case term of
    App1 home name args ->
      Can.TType home name <$> traverse go args

    Fun1 a b ->
      Can.TLambda
        <$> go a
        <*> go b

    EmptyRecord1 ->
      return $ Can.TRecord Map.empty Nothing

    Record1 fields extension ->
      do  canFields <- traverse (fieldToCanType aliasArgs) fields
          canExt <- Type.iteratedDealias <$> go extension
          return $
              case canExt of
                Can.TRecord subFields subExt ->
                    Can.TRecord (Map.union subFields canFields) subExt

                Can.TVar name ->
                    Can.TRecord canFields (Just name)

                _ ->
                    error "Used toAnnotation on a type that is not well-formed"

    Unit1 ->
      return Can.TUnit

    Tuple1 a b maybeC ->
      Can.TTuple
        <$> go a
        <*> go b
        <*> traverse go maybeC

    EmptyTagRow1 ->
      return $ Can.TTagRow Map.empty Nothing

    TagRow1 tags extension ->
      do  canTags <- traverse (traverse variableToCanType) tags
          canExt <- Type.iteratedDealias <$> variableToCanType extension
          return $
              case canExt of
                Can.TTagRow subTags subExt ->
                    Can.TTagRow (Map.union subTags canTags) subExt

                Can.TVar name ->
                    Can.TTagRow canTags (Just name)

                _ ->
                    error "Used toAnnotation on a type that is not well-formed"


fieldToCanType :: [(Variable, Can.Type)] -> Variable -> StateT NameState IO Can.FieldType
fieldToCanType aliasArgs variable =
  do  tipe <- variableToCanTypeHelp aliasArgs variable
      return (Can.FieldType 0 tipe)



-- FIND ALIAS ARGS


findAliasArg :: [(Variable, a)] -> Variable -> IO (Maybe a)
findAliasArg aliasArgs variable =
  case aliasArgs of
    [] ->
      return Nothing

    (argVar, arg) : rest ->
      do  isArg <- UF.equivalent variable argVar
          if isArg
            then return (Just arg)
            else findAliasArg rest variable



-- TO ERROR TYPE


toErrorType :: Variable -> IO ET.Type
toErrorType variable =
  do  userNames <- getVarNames variable Map.empty
      State.evalStateT (variableToErrorType variable) (makeNameState userNames)


variableToErrorType :: Variable -> StateT NameState IO ET.Type
variableToErrorType variable =
  variableToErrorTypeHelp [] variable


-- The `aliasArgs` are the (already converted) arguments of the type alias
-- whose body we are currently converting. Any variable equivalent to one of
-- them reuses the converted argument rather than converting it again. This
-- keeps the work linear in the size of the type, see `variableToCanTypeHelp`.
--
variableToErrorTypeHelp :: [(Variable, ET.Type)] -> Variable -> StateT NameState IO ET.Type
variableToErrorTypeHelp aliasArgs variable =
  do  maybeArg <- liftIO $ findAliasArg aliasArgs variable
      case maybeArg of
        Just argType ->
          return argType

        Nothing ->
          do  descriptor <- liftIO $ UF.get variable
              let mark = _mark descriptor
              if mark == occursMark
                then
                  return ET.Infinite

                else
                  do  liftIO $ UF.modify variable (\desc -> desc { _mark = occursMark })
                      errType <- contentToErrorType aliasArgs variable (_content descriptor)
                      liftIO $ UF.modify variable (\desc -> desc { _mark = mark })
                      return errType


contentToErrorType :: [(Variable, ET.Type)] -> Variable -> Content -> StateT NameState IO ET.Type
contentToErrorType aliasArgs variable content =
  case content of
    Structure term ->
        termToErrorType aliasArgs term

    FlexVar maybeName ->
      case maybeName of
        Just name ->
          return (ET.FlexVar name)

        Nothing ->
          do  name <- getFreshVarName
              liftIO $ UF.modify variable (\desc -> desc { _content = FlexVar (Just name) })
              return (ET.FlexVar name)

    FlexSuper super maybeName ->
      case maybeName of
        Just name ->
          return (ET.FlexSuper (superToSuper super) name)

        Nothing ->
          do  name <- getFreshSuperName super
              liftIO $ UF.modify variable (\desc -> desc { _content = FlexSuper super (Just name) })
              return (ET.FlexSuper (superToSuper super) name)

    RigidVar name ->
        return (ET.RigidVar name)

    RigidSuper super name ->
        return (ET.RigidSuper (superToSuper super) name)

    Alias home name args realVariable ->
        do  errArgs <- traverse (traverse (variableToErrorTypeHelp aliasArgs)) args
            let bodyArgs = zip (map snd args) (map snd errArgs)
            errType <- variableToErrorTypeHelp bodyArgs realVariable
            return (ET.Alias home name errArgs errType)

    Error ->
        return ET.Error


superToSuper :: SuperType -> ET.Super
superToSuper super =
  case super of
    Number -> ET.Number
    Comparable -> ET.Comparable
    Appendable -> ET.Appendable
    CompAppend -> ET.CompAppend


termToErrorType :: [(Variable, ET.Type)] -> FlatType -> StateT NameState IO ET.Type
termToErrorType aliasArgs term =
  let
    go = variableToErrorTypeHelp aliasArgs
  in
  case term of
    App1 home name args ->
      ET.Type home name <$> traverse go args

    Fun1 a b ->
      do  arg <- go a
          result <- go b
          return $
            case result of
              ET.Lambda arg1 arg2 others ->
                ET.Lambda arg arg1 (arg2:others)

              _ ->
                ET.Lambda arg result []

    EmptyRecord1 ->
      return $ ET.Record Map.empty ET.Closed

    Record1 fields extension ->
      do  errFields <- traverse go fields
          errExt <- ET.iteratedDealias <$> go extension
          return $
              case errExt of
                ET.Record subFields subExt ->
                    ET.Record (Map.union subFields errFields) subExt

                ET.FlexVar ext ->
                    ET.Record errFields (ET.FlexOpen ext)

                ET.RigidVar ext ->
                    ET.Record errFields (ET.RigidOpen ext)

                _ ->
                    error "Used toErrorType on a type that is not well-formed"

    Unit1 ->
      return ET.Unit

    Tuple1 a b maybeC ->
      ET.Tuple
        <$> go a
        <*> go b
        <*> traverse go maybeC

    EmptyTagRow1 ->
      return $ ET.TagRow Map.empty ET.Closed

    TagRow1 tags extension ->
      do  errTags <- traverse (traverse variableToErrorType) tags
          errExt <- ET.iteratedDealias <$> variableToErrorType extension
          return $
              case errExt of
                ET.TagRow subTags subExt ->
                    ET.TagRow (Map.union subTags errTags) subExt

                ET.FlexVar ext ->
                    ET.TagRow errTags (ET.FlexOpen ext)

                ET.RigidVar ext ->
                    ET.TagRow errTags (ET.RigidOpen ext)

                _ ->
                    error "Used toErrorType on a type that is not well-formed"



-- MANAGE FRESH VARIABLE NAMES


data NameState =
  NameState
    { _taken :: Map.Map Name.Name ()
    , _normals :: Int
    , _numbers :: Int
    , _comparables :: Int
    , _appendables :: Int
    , _compAppends :: Int
    }


makeNameState :: Map.Map Name.Name Variable -> NameState
makeNameState taken =
  NameState (Map.map (const ()) taken) 0 0 0 0 0



-- FRESH VAR NAMES


getFreshVarName :: (Monad m) => StateT NameState m Name.Name
getFreshVarName =
  do  index <- State.gets _normals
      taken <- State.gets _taken
      let (name, newIndex, newTaken) = getFreshVarNameHelp index taken
      State.modify $ \state -> state { _taken = newTaken, _normals = newIndex }
      return name


getFreshVarNameHelp :: Int -> Map.Map Name.Name () -> (Name.Name, Int, Map.Map Name.Name ())
getFreshVarNameHelp index taken =
  let
    name =
      Name.fromTypeVariableScheme index
  in
  if Map.member name taken then
    getFreshVarNameHelp (index + 1) taken
  else
    ( name, index + 1, Map.insert name () taken )



-- FRESH SUPER NAMES


getFreshSuperName :: (Monad m) => SuperType -> StateT NameState m Name.Name
getFreshSuperName super =
  case super of
    Number ->
      getFreshSuper "number" _numbers (\index state -> state { _numbers = index })

    Comparable ->
      getFreshSuper "comparable" _comparables (\index state -> state { _comparables = index })

    Appendable ->
      getFreshSuper "appendable" _appendables (\index state -> state { _appendables = index })

    CompAppend ->
      getFreshSuper "compappend" _compAppends (\index state -> state { _compAppends = index })


getFreshSuper :: (Monad m) => Name.Name -> (NameState -> Int) -> (Int -> NameState -> NameState) -> StateT NameState m Name.Name
getFreshSuper prefix getter setter =
  do  index <- State.gets getter
      taken <- State.gets _taken
      let (name, newIndex, newTaken) = getFreshSuperHelp prefix index taken
      State.modify (\state -> setter newIndex state { _taken = newTaken })
      return name


getFreshSuperHelp :: Name.Name -> Int -> Map.Map Name.Name () -> (Name.Name, Int, Map.Map Name.Name ())
getFreshSuperHelp prefix index taken =
  let
    name =
      Name.fromTypeVariable prefix index
  in
    if Map.member name taken then
      getFreshSuperHelp prefix (index + 1) taken

    else
      ( name, index + 1, Map.insert name () taken )



-- GET ALL VARIABLE NAMES


getVarNames :: Variable -> Map.Map Name.Name Variable -> IO (Map.Map Name.Name Variable)
getVarNames var takenNames =
  do  (Descriptor content rank mark copy) <- UF.get var
      if mark == getVarNamesMark
        then return takenNames
        else
        do  UF.set var (Descriptor content rank getVarNamesMark copy)
            case content of
              Error ->
                return takenNames

              FlexVar maybeName ->
                case maybeName of
                  Nothing ->
                    return takenNames

                  Just name ->
                    addName 0 name var (FlexVar . Just) takenNames

              FlexSuper super maybeName ->
                case maybeName of
                  Nothing ->
                    return takenNames

                  Just name ->
                    addName 0 name var (FlexSuper super . Just) takenNames

              RigidVar name ->
                addName 0 name var RigidVar takenNames

              RigidSuper super name ->
                addName 0 name var (RigidSuper super) takenNames

              Alias _ _ args _ ->
                foldrM getVarNames takenNames (map snd args)

              Structure flatType ->
                case flatType of
                  App1 _ _ args ->
                    foldrM getVarNames takenNames args

                  Fun1 arg body ->
                    getVarNames arg =<< getVarNames body takenNames

                  EmptyRecord1 ->
                    return takenNames

                  Record1 fields extension ->
                    getVarNames extension =<<
                      foldrM getVarNames takenNames (Map.elems fields)

                  Unit1 ->
                    return takenNames

                  Tuple1 a b Nothing ->
                    getVarNames a =<< getVarNames b takenNames

                  Tuple1 a b (Just c) ->
                    getVarNames a =<< getVarNames b =<< getVarNames c takenNames

                  EmptyTagRow1 ->
                    return takenNames

                  TagRow1 tags extension ->
                    getVarNames extension =<<
                      foldrM (\args names -> foldrM getVarNames names args) takenNames (Map.elems tags)



-- REGISTER NAME / RENAME DUPLICATES


addName :: Int -> Name.Name -> Variable -> (Name.Name -> Content) -> Map.Map Name.Name Variable -> IO (Map.Map Name.Name Variable)
addName index givenName var makeContent takenNames =
  let
    indexedName =
      Name.fromTypeVariable givenName index
  in
    case Map.lookup indexedName takenNames of
      Nothing ->
        do  if indexedName == givenName then return () else
              UF.modify var $ \(Descriptor _ rank mark copy) ->
                Descriptor (makeContent indexedName) rank mark copy
            return $ Map.insert indexedName var takenNames

      Just otherVar ->
        do  same <- UF.equivalent var otherVar
            if same
              then return takenNames
              else addName (index + 1) givenName var makeContent takenNames
