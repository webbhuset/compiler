{-# LANGUAGE OverloadedStrings #-}
module Nitpick.AsyncImports
  ( check
  , Error(..)
  , toReport
  )
  where


import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report



-- CHECK
--
-- A value from a module brought in with `import async` lives in a chunk
-- that is fetched the first time it is used, and the fetch is awaited by
-- re-running whatever asked for it: update, view, init, subscriptions.
--
-- A top-level definition written without arguments has no such second
-- chance. It is evaluated the moment its bundle loads, before the program
-- exists, so a chunk lookup there could never be awaited. This pass
-- rejects exactly that: an async reference that is not under a lambda.


data Error =
  ForcedAtLoad
    { _region :: A.Region
    , _definition :: Name.Name
    , _module :: ModuleName.Canonical
    , _value :: Name.Name
    }


type Asyncs =
  Set.Set ModuleName.Canonical


check :: Can.Module -> Either (NE.List Error) ()
check (Can.Module home _ _ decls _ _ _ overloads _ _ asyncs) =
  if Set.null asyncs then
    Right ()
  else
    case checkDecls asyncs (isFunctionShaped home overloads) decls [] of
      [] ->
        Right ()

      e:es ->
        Left (NE.List e es)


-- A definition that asks for overloads takes them as leading arguments, so
-- it compiles to a function even when it is written without any. Optimize
-- adds those arguments later; this pass runs before that and has to ask.
isFunctionShaped :: ModuleName.Canonical -> Can.Overloads -> Name.Name -> Bool
isFunctionShaped home overloads =
  \name -> Map.member (home, name) (Can._constrained overloads)


checkDecls :: Asyncs -> (Name.Name -> Bool) -> Can.Decls -> [Error] -> [Error]
checkDecls asyncs isFunc decls errors =
  case decls of
    Can.Declare def subDecls ->
      checkTopDef asyncs isFunc def (checkDecls asyncs isFunc subDecls errors)

    Can.DeclareRec def defs subDecls ->
      checkTopDef asyncs isFunc def $
        foldr (checkTopDef asyncs isFunc) (checkDecls asyncs isFunc subDecls errors) defs

    Can.SaveTheEnvironment ->
      errors


checkTopDef :: Asyncs -> (Name.Name -> Bool) -> Can.Def -> [Error] -> [Error]
checkTopDef asyncs isFunc def errors =
  case defShape def of
    (name, True, _) | isFunc name ->
      errors

    (_, True, _) ->
      errors

    (name, False, body) ->
      checkExpr asyncs name body errors


-- The name, whether it is written with arguments, and the body.
defShape :: Can.Def -> (Name.Name, Bool, Can.Expr)
defShape def =
  case def of
    Can.Def (A.At _ name) args body ->
      (name, not (null args), body)

    Can.TypedDef (A.At _ name) _ args body _ ->
      (name, not (null args), body)



-- WALK THE EAGER PART OF A DEFINITION
--
-- Descend through everything that runs when the definition is evaluated,
-- and stop at every lambda: what is under one runs later, which is exactly
-- when a chunk can be awaited.


checkExpr :: Asyncs -> Name.Name -> Can.Expr -> [Error] -> [Error]
checkExpr asyncs def (A.At region expression) errors =
  let
    recurse expr es = checkExpr asyncs def expr es

    foreign_ home name es =
      if Set.member home asyncs
        then ForcedAtLoad region def home name : es
        else es
  in
  case expression of
    Can.VarForeign home name _ -> foreign_ home name errors
    Can.VarTag home name _ -> foreign_ home name errors
    Can.VarOperator _ home name _ -> foreign_ home name errors
    Can.VarOverload _ (home, name) _ -> foreign_ home name errors
    Can.VarConstrained _ home name _ _ -> foreign_ home name errors
    Can.Binop _ home name _ left right ->
      foreign_ home name (recurse left (recurse right errors))

    -- everything under a lambda runs when it is called, which the runtime
    -- can retry, so it is not this pass's business
    Can.Lambda _ _ -> errors

    Can.VarLocal _ -> errors
    Can.VarTopLevel _ _ -> errors
    Can.VarKernel _ _ -> errors
    Can.VarCtor _ _ _ _ _ -> errors
    Can.VarDebug _ _ _ -> errors
    Can.Widen inner -> recurse inner errors
    Can.Chr _ -> errors
    Can.Str _ -> errors
    Can.Int _ -> errors
    Can.Float _ -> errors
    Can.List entries -> foldr recurse errors entries
    Can.Negate expr -> recurse expr errors
    Can.Call func args -> recurse func (foldr recurse errors args)
    Can.If branches finally ->
      foldr (\(c, b) es -> recurse c (recurse b es)) (recurse finally errors) branches
    Can.Let letDef body -> checkLetDef asyncs letDef (recurse body errors)
    Can.LetRec defs body -> foldr (checkLetDef asyncs) (recurse body errors) defs
    Can.LetDestruct _ expr body -> recurse expr (recurse body errors)
    Can.Case expr branches ->
      recurse expr (foldr (\(Can.CaseBranch _ b) es -> recurse b es) errors branches)
    Can.Accessor _ -> errors
    Can.Access expr _ -> recurse expr errors
    Can.Update _ expr fields ->
      recurse expr (Map.foldr (\(Can.FieldUpdate _ e) es -> recurse e es) errors fields)
    Can.Record fields -> Map.foldr recurse errors fields
    Can.Unit -> errors
    Can.Tuple a b maybeC -> recurse a (recurse b (foldr recurse errors maybeC))
    Can.Shader _ _ -> errors
    Can.Css _ _ -> errors


-- A let-bound function body runs when it is called, like a lambda's.
checkLetDef :: Asyncs -> Can.Def -> [Error] -> [Error]
checkLetDef asyncs def errors =
  case defShape def of
    (_, True, _) -> errors
    (name, False, body) -> checkExpr asyncs name body errors



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source (ForcedAtLoad region def home value) =
  let
    modul = ModuleName._module home
    qualified = Name.toChars modul ++ "." ++ Name.toChars value
  in
  Report.Report "ASYNC VALUE AT LOAD TIME" region [] $
    Code.toSnippet source region Nothing
      (
        D.reflow $
          "This uses " ++ qualified ++ ", but `" ++ Name.toChars def
          ++ "` is evaluated as soon as the bundle loads:"
      ,
        D.stack
          [ D.reflow $
              "You imported " ++ Name.toChars modul
              ++ " with `import async`, so its code arrives in a separate file,\
                 \ fetched the first time something needs it. The wait happens by\
                 \ running that code again once the file is there -- which works\
                 \ inside update, view, init and subscriptions, but not here.\
                 \ A definition written without arguments runs before the program\
                 \ exists, and there is nothing to run again."
          , D.reflow $
              "Give it an argument, so the reference happens when it is called:"
          , D.indent 4 $ D.dullyellow $ D.fromChars $
              Name.toChars def ++ " arg =\n        " ++ qualified ++ " arg"
          , D.reflow $
              "If it really has to be ready at load time, import "
              ++ Name.toChars modul ++ " without `async` instead."
          ]
      )
