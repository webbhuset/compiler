{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Overload
  ( Error(..)
  , toReport
  )
  where


import qualified Data.Name as Name

import qualified AST.Canonical as Can
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
  = NoDefinition A.Region ModuleName.Canonical Name.Name Can.Type
  | NoClause A.Region ModuleName.Canonical Name.Name Name.Name (Maybe Can.Type)
  | Ambiguous A.Region ModuleName.Canonical Name.Name (Maybe Can.Type)



-- TO REPORT


toReport :: Code.Source -> L.Localizer -> Error -> Report.Report
toReport source localizer err =
  case err of
    NoDefinition region home name dispatched ->
      Report.Report "NO DEFINITION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "There is no definition of " ++ toQualified home name ++ " for this type:"
          ,
            D.stack
              [ D.indent 4 $ RT.canToDoc localizer RT.None dispatched
              , D.reflow $
                  "Add one in module " ++ homeOf dispatched home ++ ", or in module "
                  ++ Name.toChars (ModuleName._module home) ++ "."
              ]
          )

    NoClause region home name var wanted ->
      Report.Report "MISSING WHERE CLAUSE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "This needs " ++ toQualified home name ++ " on `" ++ Name.toChars var
              ++ "`, which could be any type:"
          ,
            D.stack
              [ D.reflow $
                  "So the signature above has to say that it needs it, by adding this line\
                  \ under it:"
              , D.indent 4 $ D.hang 4 $ D.sep $
                  [ D.dullyellow (D.fromChars ("where " ++ toQualified home name)), ":" ]
                  ++ case wanted of
                       Just tipe -> [ RT.canToDoc localizer RT.None tipe ]
                       Nothing   -> [ "..." ]
              ]
          )

    Ambiguous region home name maybeDispatched ->
      Report.Report "AMBIGUOUS OVERLOAD" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I cannot tell which definition of " ++ toQualified home name
              ++ " this use needs:"
          ,
            case maybeDispatched of
              Nothing ->
                D.reflow $
                  "Nothing here says what type it is used at. Adding a type annotation\
                  \ that says which type you mean should settle it."

              Just dispatched ->
                D.stack
                  [ D.reflow "The type it dispatches on came out as:"
                  , D.indent 4 $ RT.canToDoc localizer RT.None dispatched
                  , D.reflow $
                      "which is not a specific enough type to pick a definition. Adding a\
                      \ type annotation that says which type you mean should settle it."
                  ]
          )


toQualified :: ModuleName.Canonical -> Name.Name -> String
toQualified home name =
  Name.toChars (ModuleName._module home) ++ "." ++ Name.toChars name


homeOf :: Can.Type -> ModuleName.Canonical -> String
homeOf tipe fallback =
  case tipe of
    Can.TType home _ _    -> Name.toChars (ModuleName._module home)
    Can.TAlias home _ _ _ -> Name.toChars (ModuleName._module home)
    _                     -> Name.toChars (ModuleName._module fallback)
