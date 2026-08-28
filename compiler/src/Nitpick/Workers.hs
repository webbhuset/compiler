{-# LANGUAGE OverloadedStrings #-}
module Nitpick.Workers
  ( check
  , Error(..)
  , toReport
  )
  where


import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report



-- CHECK
--
-- Browser.Worker.spawn compiles the referenced worker program into a
-- separate bundle, so the compiler must see, at every use, which top-level
-- value is being spawned. This pass rejects any use of spawn that is not a
-- direct, fully applied call whose first argument is a direct reference to
-- a top-level value, e.g. `Worker.spawn Counter.main args handlers`.


data Error
  = NotCalledDirectly A.Region
  | BadProgramArg A.Region


check :: Can.Module -> Either (NE.List Error) ()
check (Can.Module _ _ _ decls _ _ _ _ _) =
  case checkDecls decls [] of
    [] ->
      Right ()

    e:es ->
      Left (NE.List e es)


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
