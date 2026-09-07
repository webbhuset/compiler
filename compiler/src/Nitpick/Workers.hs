{-# LANGUAGE OverloadedStrings #-}
module Nitpick.Workers
  ( check
  , Error(..)
  , BoundaryParam(..)
  , boundaryProblems
  , toReport
  )
  where


import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE

import qualified AST.Canonical as Can
import qualified AST.Utils.Type as Type
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report
import qualified Type.Portable as Portable



-- CHECK
--
-- Two things about Browser.Worker programs:
--
--   1. Browser.Worker.spawn compiles the referenced worker program into a
--      separate bundle, so every use must be a direct, fully applied call
--      whose first argument names a top-level value.
--
--   2. The values that cross the worker boundary -- a program's args,
--      toParent, and msg -- are passed as structured clones, so their types
--      must be portable (no functions, no kernel/effect types). This is
--      checked at every top-level value whose type is a Worker.Program, so
--      the error lands on the worker's own definition.


data Error
  = NotCalledDirectly A.Region
  | BadProgramArg A.Region
  | NonPortableBoundary A.Region Name.Name BoundaryParam Portable.Problem


data BoundaryParam
  = ArgsParam
  | ToParentParam
  | MsgParam
  deriving (Eq, Show)


check :: Portable.Info -> Map.Map Name.Name Can.Annotation -> Can.Module -> Either (NE.List Error) ()
check info annotations (Can.Module home _ _ decls unions _ _ _ _) =
  let
    boundary = boundaryProblems info home unions (topLevelTyped annotations decls)
  in
  case checkDecls decls boundary of
    [] ->
      Right ()

    e:es ->
      Left (NE.List e es)



-- BOUNDARY PORTABILITY


boundaryProblems
  :: Portable.Info
  -> ModuleName.Canonical
  -> Map.Map Name.Name Can.Union
  -> [(A.Region, Name.Name, Can.Annotation)]
  -> [Error]
boundaryProblems info home unions typedValues =
  concatMap perValue typedValues
  where
    perValue (region, name, Can.Forall _ tipe) =
      case programArgs tipe of
        Nothing ->
          []

        Just (args, toParent, msg) ->
          concat
            [ perParam region name ArgsParam args
            , perParam region name ToParentParam toParent
            , perParam region name MsgParam msg
            ]

    perParam region name param tipe =
      case Portable.checkPortable info home unions tipe of
        Nothing      -> []
        Just problem -> [NonPortableBoundary region name param problem]


-- The three boundary arguments of a Worker.Program annotation (args,
-- toParent, msg), or Nothing if the type is not a worker program. The
-- fourth parameter (model) never crosses, so it is ignored.
programArgs :: Can.Type -> Maybe (Can.Type, Can.Type, Can.Type)
programArgs tipe =
  case tipe of
    Can.TAlias _ _ args aliased ->
      programArgs (Type.dealias args aliased)

    Can.TType home name [args, toParent, msg, _model]
      | home == ModuleName.workers && name == "Program" ->
          Just (args, toParent, msg)

    _ ->
      Nothing


topLevelTyped :: Map.Map Name.Name Can.Annotation -> Can.Decls -> [(A.Region, Name.Name, Can.Annotation)]
topLevelTyped annotations decls =
  [ (region, name, annotation)
  | def <- topLevelDefs decls
  , let (region, name) = defRegionName def
  , annotation <- Maybe.maybeToList (Map.lookup name annotations)
  ]


topLevelDefs :: Can.Decls -> [Can.Def]
topLevelDefs decls =
  case decls of
    Can.Declare def rest ->
      def : topLevelDefs rest

    Can.DeclareRec def defs rest ->
      def : defs ++ topLevelDefs rest

    Can.SaveTheEnvironment ->
      []


defRegionName :: Can.Def -> (A.Region, Name.Name)
defRegionName def =
  case def of
    Can.Def (A.At region name) _ _        -> (region, name)
    Can.TypedDef (A.At region name) _ _ _ _ -> (region, name)



-- SPAWN IS CALLED DIRECTLY


checkDecls :: Can.Decls -> [Error] -> [Error]
checkDecls decls errors =
  case decls of
    Can.Declare def subDecls ->
      checkDef def (checkDecls subDecls errors)

    Can.DeclareRec def defs subDecls ->
      checkDef def (foldr checkDef (checkDecls subDecls errors) defs)

    Can.SaveTheEnvironment ->
      errors


checkDef :: Can.Def -> [Error] -> [Error]
checkDef def errors =
  case def of
    Can.Def _ _ expr ->
      checkExpr expr errors

    Can.TypedDef _ _ _ expr _ ->
      checkExpr expr errors


isSpawn :: ModuleName.Canonical -> Name.Name -> Bool
isSpawn home name =
  home == ModuleName.workers && name == Name.fromChars "spawn"


checkExpr :: Can.Expr -> [Error] -> [Error]
checkExpr (A.At region expression) errors =
  case expression of
    Can.Call (A.At _ (Can.VarForeign home name _)) args | isSpawn home name ->
      case args of
        [A.At _ (Can.VarForeign _ _ _), a2, a3] ->
          checkExpr a2 (checkExpr a3 errors)

        [A.At _ (Can.VarTopLevel _ _), a2, a3] ->
          checkExpr a2 (checkExpr a3 errors)

        [A.At argRegion _, a2, a3] ->
          BadProgramArg argRegion : checkExpr a2 (checkExpr a3 errors)

        _ ->
          NotCalledDirectly region : foldr checkExpr errors args

    Can.VarForeign home name _ | isSpawn home name ->
      NotCalledDirectly region : errors

    Can.VarLocal _ -> errors
    Can.VarTopLevel _ _ -> errors
    Can.VarKernel _ _ -> errors
    Can.VarForeign _ _ _ -> errors
    Can.VarCtor _ _ _ _ _ -> errors
    Can.VarTag _ _ _ -> errors
    Can.VarDebug _ _ _ -> errors
    Can.VarOperator _ _ _ _ -> errors
    Can.Chr _ -> errors
    Can.Str _ -> errors
    Can.Int _ -> errors
    Can.Float _ -> errors
    Can.List entries -> foldr checkExpr errors entries
    Can.Negate expr -> checkExpr expr errors
    Can.Binop _ _ _ _ left right -> checkExpr left (checkExpr right errors)
    Can.Lambda _ body -> checkExpr body errors
    Can.Call func args -> checkExpr func (foldr checkExpr errors args)
    Can.If branches finally ->
      foldr (\(c, b) es -> checkExpr c (checkExpr b es)) (checkExpr finally errors) branches
    Can.Let def body -> checkDef def (checkExpr body errors)
    Can.LetRec defs body -> foldr checkDef (checkExpr body errors) defs
    Can.LetDestruct _ expr body -> checkExpr expr (checkExpr body errors)
    Can.Case expr branches ->
      checkExpr expr (foldr (\(Can.CaseBranch _ b) es -> checkExpr b es) errors branches)
    Can.Accessor _ -> errors
    Can.Access expr _ -> checkExpr expr errors
    Can.Update _ expr fields ->
      checkExpr expr (Map.foldr (\(Can.FieldUpdate _ e) es -> checkExpr e es) errors fields)
    Can.Record fields -> Map.foldr checkExpr errors fields
    Can.Unit -> errors
    Can.Tuple a b maybeC ->
      checkExpr a (checkExpr b (foldr checkExpr errors maybeC))
    Can.Shader _ _ -> errors
    Can.Css _ _ -> errors



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source err =
  case err of
    NotCalledDirectly region ->
      Report.Report "BAD WORKER SPAWN" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Browser.Worker.spawn must be called directly, with all three arguments:"
          ,
            D.stack
              [ D.reflow $
                  "The compiler turns the spawned worker program into a separate\
                  \ JavaScript file, so it needs to see the whole call at compile time.\
                  \ Write it like this:"
              , D.indent 4 $ D.dullyellow $
                  "Worker.spawn Counter.main args handlers"
              , D.reflow $
                  "Passing spawn around as a function or applying it partially\
                  \ hides the worker program from the compiler."
              ]
          )

    BadProgramArg region ->
      Report.Report "BAD WORKER PROGRAM" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "The first argument of Browser.Worker.spawn must be a direct reference to a\
              \ top-level worker program:"
          ,
            D.stack
              [ D.reflow $
                  "The compiler turns the worker program into a separate JavaScript\
                  \ file, so it must know at compile time exactly which top-level\
                  \ value is being spawned, like:"
              , D.indent 4 $ D.dullyellow $
                  "Worker.spawn Counter.main args handlers"
              , D.reflow $
                  "Computing the program, passing it through a function, or picking\
                  \ it from a data structure does not work."
              ]
          )

    NonPortableBoundary region valueName param problem ->
      Report.Report "NON-PORTABLE WORKER MESSAGE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "The " ++ paramDescription param ++ " of the worker program `"
              ++ Name.toChars valueName ++ "` cannot cross the worker boundary:"
          ,
            D.stack
              [ problemDoc problem
              , D.reflow $
                  "Values crossing to or from a worker are copied by structured clone,\
                  \ so they must not contain functions or kernel types like Cmd, Task,\
                  \ or Json.Decode.Decoder."
              ]
          )


paramDescription :: BoundaryParam -> String
paramDescription param =
  case param of
    ArgsParam     -> "argument type (args)"
    ToParentParam -> "toParent type"
    MsgParam      -> "message type (msg)"


problemDoc :: Portable.Problem -> D.Doc
problemDoc problem =
  case problem of
    Portable.PFunction ->
      D.reflow "It contains a function, and functions cannot be structured-cloned."

    Portable.PBadAtom (_, name) ->
      D.reflow $
        "It contains `" ++ Name.toChars name ++ "`, which cannot cross the worker\
        \ boundary -- it is defined in a module with kernel code, so the compiler\
        \ cannot guarantee it survives a structured clone."

    Portable.PTypeVar name ->
      D.reflow $
        "It is not concrete: the type variable `" ++ Name.toChars name ++ "` could be\
        \ anything, including a function. Give the worker program a concrete type\
        \ annotation."

    Portable.PExtensibleRecord ->
      D.reflow
        "It has an open record type. Only closed records can cross the worker boundary."
