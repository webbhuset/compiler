{-# LANGUAGE OverloadedStrings #-}
module Tool.Problem
  ( Problem(..)
  , toReport
  )
  where


import qualified Data.List as List

import qualified AST.Prim.Module as Module
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Error as Error
import qualified Root as R



-- PROBLEMS


data Problem
  = NoOutline
  | BadDetails Exit.Details
  | UnknownCommand String
  | BadArgs String String
  | BadName String String
  | ModuleNotFound Module.Name [FilePath]
  | FileNotFound FilePath
  | NothingAt String
  | ModuleInPackage Module.Name String
  | NotFound String String [String]
  | BadModule R.Root Error.Module
  | BadBuild Exit.Repl
  | BadMake Exit.Make


toReport :: Problem -> Help.Report
toReport problem =
  case problem of
    NoOutline ->
      Help.report "NO elm.json FILE" Nothing
        "`elm tool` looks at an Elm project, but I cannot find an elm.json file in this\
        \ directory or any directory above it."
        []

    BadDetails details ->
      Exit.replToReport (Exit.ReplBadDetails details)

    UnknownCommand command ->
      Help.report "UNKNOWN COMMAND" Nothing
        ("There is no `elm tool " ++ command ++ "` command. Run `elm tool --help` to see\
        \ the commands there are.")
        []

    BadArgs command usage ->
      Help.report "BAD ARGUMENTS" Nothing
        ("The `" ++ command ++ "` command is used like this:")
        [ D.indent 4 (D.green (D.fromChars usage)) ]

    BadName expected given ->
      Help.report "BAD NAME" Nothing
        ("I was expecting " ++ expected ++ ", but got `" ++ given ++ "`.")
        []

    ModuleNotFound name dirs ->
      Help.report "MODULE NOT FOUND" Nothing
        ("I cannot find a `" ++ Module.toChars name ++ "` module. I looked for "
          ++ Module.toFilePath name ++ ".elm in these source directories:")
        [ D.indent 4 (D.vcat (map D.fromChars dirs)) ]

    FileNotFound path ->
      Help.report "FILE NOT FOUND" Nothing
        ("I cannot find a file at " ++ path ++ ".")
        []

    NothingAt position ->
      Help.report "NOTHING THERE" Nothing
        ("There is no expression at " ++ position ++ ". Lines and columns count from 1,\
        \ and the position has to be inside a definition.")
        []

    ModuleInPackage name pkg ->
      Help.report "MODULE IN A PACKAGE" Nothing
        ("The `" ++ Module.toChars name ++ "` module comes from the " ++ pkg
          ++ " package, but I cannot find its docs.json or its interface in the cache.\
          \ Running `elm make` once may help.")
        []

    NotFound what name suggestions ->
      Help.report "NOT FOUND" Nothing
        ("I cannot find " ++ what ++ " named `" ++ name ++ "`.")
        ( case suggestions of
            [] -> []
            _  -> [ D.reflow "These names are close:"
                  , D.indent 4 (D.vcat (map (D.green . D.fromChars) (List.take 4 suggestions)))
                  ]
        )

    BadModule root err ->
      Help.compilerReport root err []

    BadBuild repl ->
      Exit.replToReport repl

    BadMake make ->
      Exit.makeToReport make
