{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Compile
  ( Artifacts(..)
  , compile
  )
  where


import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Canonicalize.Module as Canonicalize
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Nitpick.PatternMatches as PatternMatches
import qualified Nitpick.Workers as Workers
import qualified Optimize.Module as Optimize
import qualified Reporting.Error as E
import qualified Reporting.Result as R
import qualified Reporting.Render.Type.Localizer as Localizer
import qualified Type.Comparable as Comparable
import qualified Type.Overload as Overload
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type

import System.IO.Unsafe (unsafePerformIO)



-- COMPILE


data Artifacts =
  Artifacts
    { _modul :: Can.Module
    , _types :: Map.Map Name.Name Can.Annotation
    , _graph :: Opt.LocalGraph
    , _comparables :: Set.Set Comparable.Atom
    }


compile :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Artifacts
compile pkg ifaces modul =
  do  canonical   <- canonicalize pkg ifaces modul
      let comparables = Comparable.compute ifaces canonical
      annotations <- typeCheck comparables modul canonical
      ()          <- resolveOverloads modul canonical
      ()          <- nitpick canonical
      objects     <- optimize modul annotations canonical
      return (Artifacts canonical annotations objects (Comparable._atoms comparables))



-- PHASES


canonicalize :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Can.Module
canonicalize pkg ifaces modul =
  case snd $ R.run $ Canonicalize.canonicalize pkg ifaces modul of
    Right canonical ->
      Right canonical

    Left errors ->
      Left $ E.BadNames errors


typeCheck :: Comparable.Info -> Src.Module -> Can.Module -> Either E.Error (Map.Map Name.Name Can.Annotation)
typeCheck comparables modul canonical@(Can.Module _ _ _ _ _ _ _ overloads _ _) =
  case unsafePerformIO (Comparable.register comparables >> (Type.run overloads =<< Type.constrain canonical)) of
    Right annotations ->
      Right annotations

    Left errors ->
      Left (E.BadTypes (Localizer.fromModule modul) errors)


-- Runs after typeCheck, because which definition each overloaded use site
-- means is decided by the type the solver gave it.
resolveOverloads :: Src.Module -> Can.Module -> Either E.Error ()
resolveOverloads modul (Can.Module home _ _ _ _ _ _ overloads _ _) =
  case unsafePerformIO (Overload.resolveModule home overloads) of
    [] ->
      Right ()

    e:es ->
      Left (E.BadOverloads (Localizer.fromModule modul) (NE.List e es))


nitpick :: Can.Module -> Either E.Error ()
nitpick canonical =
  do  case PatternMatches.check canonical of
        Right () -> Right ()
        Left errors -> Left (E.BadPatterns errors)
      case Workers.check canonical of
        Right () -> Right ()
        Left errors -> Left (E.BadWorkers errors)


optimize :: Src.Module -> Map.Map Name.Name Can.Annotation -> Can.Module -> Either E.Error Opt.LocalGraph
optimize modul annotations canonical =
  case snd $ R.run $ Optimize.optimize annotations canonical of
    Right localGraph ->
      Right localGraph

    Left errors ->
      Left (E.BadMains (Localizer.fromModule modul) errors)
