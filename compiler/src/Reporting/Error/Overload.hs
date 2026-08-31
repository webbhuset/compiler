{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Overload
  ( Error(..)
  , toReport
  )
  where



import qualified AST.Canonical as Can
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report



-- ERRORS
--
-- Reported after type checking, when the type a use site was used at is known
-- but does not lead to a definition.


data Error
  = NoDefinition A.Region ModuleName.Canonical N.Name Can.Type Can.Type
  | Ambiguous A.Region ModuleName.Canonical N.Name Can.Type



-- TO REPORT


toReport :: Code.Source -> L.Localizer -> Error -> Report.Report
toReport source localizer err =
  case err of
    NoDefinition region home name dispatchType wanted ->
      Report.Report "NO DEFINITION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "There is no definition of " ++ toQualified home name ++ " for this type:"
          ,
            D.stack
              [ D.reflow $
                  "Using it here needs one with this signature:"
              , D.indent 4 $ D.hang 4 $ D.sep $
                  [ D.dullyellow (D.fromChars (toQualified home name)), ":" ]
                  ++ [ RT.canToDoc localizer RT.None wanted ]
              , D.reflow $
                  "Add it to module " ++ homeOf dispatchType home ++ ", or to module "
                  ++ Module.toChars (ModuleName._module home) ++ "."
              ]
          )

    Ambiguous region home name dispatchType ->
      Report.Report "AMBIGUOUS OVERLOAD" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I cannot tell which definition of " ++ toQualified home name
              ++ " this use needs:"
          ,
            D.stack
              [ D.reflow $
                  "The type it dispatches on came out as:"
              , D.indent 4 $ RT.canToDoc localizer RT.None dispatchType
              , D.reflow $
                  "which is not a specific enough type to pick a definition. Adding a type\
                  \ annotation that says which type you mean should settle it."
              ]
          )


toQualified :: ModuleName.Canonical -> N.Name -> String
toQualified home name =
  Module.toChars (ModuleName._module home) ++ "." ++ N.toChars name


homeOf :: Can.Type -> ModuleName.Canonical -> String
homeOf tipe fallback =
  case tipe of
    Can.TType home _ _    -> Module.toChars (ModuleName._module home)
    Can.TAlias home _ _ _ -> Module.toChars (ModuleName._module home)
    _                     -> Module.toChars (ModuleName._module fallback)
