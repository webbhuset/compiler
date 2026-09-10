{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where


import Control.Monad (unless)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Exit (exitFailure)

import qualified AST.Canonical as Can
import qualified Data.Index as Index
import qualified Data.Name as Name
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Nitpick.Workers as Workers
import qualified Reporting.Annotation as A
import qualified Type.Portable as Portable



main :: IO ()
main =
  do  let outcomes = map runCase cases ++ computeCases ++ boundaryCases
      mapM_ report outcomes
      unless (all snd outcomes) exitFailure


report :: (String, Bool) -> IO ()
report (label, ok) =
  putStrLn ((if ok then "PASS  " else "FAIL  ") ++ label)


data Case =
  Case String Portable.Info (Map.Map Name.Name Can.Union) Can.Type (Maybe Portable.Problem)


runCase :: Case -> (String, Bool)
runCase (Case label info unions tipe expected) =
  (label, Portable.checkPortable info home unions tipe == expected)


home :: ModuleName.Canonical
home =
  ModuleName.Canonical Pkg.dummyName (Name.fromChars "Test")


noInfo :: Portable.Info
noInfo =
  Portable.Info Set.empty


noUnions :: Map.Map Name.Name Can.Union
noUnions =
  Map.empty



-- FIXTURES


int :: Can.Type
int =
  Can.TType ModuleName.basics Name.int []


string :: Can.Type
string =
  Can.TType ModuleName.string Name.string []


lambda :: Can.Type
lambda =
  Can.TLambda int int


listOf :: Can.Type -> Can.Type
listOf element =
  Can.TType ModuleName.list Name.list [element]


recordOf :: [(String, Can.Type)] -> Maybe Name.Name -> Can.Type
recordOf fields ext =
  Can.TRecord (Map.fromList [ (Name.fromChars k, Can.FieldType 0 t) | (k, t) <- fields ]) ext


union :: [Name.Name] -> [Can.Type] -> Can.Union
union vars payloads =
  Can.Union vars [ Can.Ctor "Ctor" Index.first (length payloads) payloads ] 1 Can.Normal


local :: Name.Name -> [Can.Type] -> Can.Type
local name args =
  Can.TType home name args


foreign_ :: Name.Name -> [Can.Type] -> Can.Type
foreign_ name args =
  Can.TType otherHome name args


otherHome :: ModuleName.Canonical
otherHome =
  ModuleName.Canonical Pkg.dummyName (Name.fromChars "Other")


-- type Tree a = Ctor a (List (Tree a))
treeUnions :: Map.Map Name.Name Can.Union
treeUnions =
  Map.fromList [ ("Tree", union ["a"] [ Can.TVar "a", listOf (local "Tree" [Can.TVar "a"]) ]) ]

-- type Loop = Ctor (List Loop)
loopUnions :: Map.Map Name.Name Can.Union
loopUnions =
  Map.fromList [ ("Loop", union [] [ listOf (local "Loop" []) ]) ]

-- type Weird = Ctor (Int -> Int)
weirdUnions :: Map.Map Name.Name Can.Union
weirdUnions =
  Map.fromList [ ("Weird", union [] [ lambda ]) ]



-- CASES


cases :: [Case]
cases =
  -- scalars, containers, tuples, records
  [ Case "scalar Int is portable" noInfo noUnions int Nothing
  , Case "scalar String is portable" noInfo noUnions string Nothing
  , Case "a bare function is not portable" noInfo noUnions lambda (Just Portable.PFunction)
  , Case "List of Int is portable" noInfo noUnions (listOf int) Nothing
  , Case "List of functions is not portable" noInfo noUnions (listOf lambda) (Just Portable.PFunction)
  , Case "tuple of portable fields is portable" noInfo noUnions (Can.TTuple int string Nothing) Nothing
  , Case "record with a function field is not portable"
      noInfo noUnions (recordOf [("run", lambda), ("n", int)] Nothing) (Just Portable.PFunction)
  , Case "closed record of portable fields is portable"
      noInfo noUnions (recordOf [("n", int)] Nothing) Nothing
  , Case "an open record is not portable"
      noInfo noUnions (recordOf [("n", int)] (Just "r")) (Just Portable.PExtensibleRecord)
  , Case "a free type variable at the boundary is not portable"
      noInfo noUnions (Can.TVar "a") (Just (Portable.PTypeVar "a"))

  -- local unions, parametric verdicts, coinductive recursion
  , Case "generic local type used concretely is portable"
      noInfo treeUnions (local "Tree" [int]) Nothing
  , Case "generic local type with a function argument is not portable"
      noInfo treeUnions (local "Tree" [lambda]) (Just Portable.PFunction)
  , Case "a recursive local type is portable"
      noInfo loopUnions (local "Loop" []) Nothing
  , Case "a local type wrapping a function is not portable"
      noInfo weirdUnions (local "Weird" []) (Just Portable.PFunction)

  -- foreign atoms via the stored verdict set
  , Case "a foreign type absent from the set is portable"
      noInfo noUnions (foreign_ "Widget" []) Nothing
  , Case "a foreign type in the non-portable set is rejected"
      (Portable.Info (Set.singleton (otherHome, "Cmd"))) noUnions (foreign_ "Cmd" [int])
      (Just (Portable.PBadAtom (otherHome, "Cmd")))
  , Case "a portable foreign container with a function argument is not portable"
      noInfo noUnions (foreign_ "Box" [lambda]) (Just Portable.PFunction)
  ]



-- COMPUTE (definition-site verdicts)


nonportablesOf :: Set.Set Portable.Atom -> Bool -> ModuleName.Canonical -> Map.Map Name.Name Can.Union -> Set.Set Portable.Atom
nonportablesOf imported usesKernel h unions =
  Portable._nonportables (Portable.computeLocals imported usesKernel h unions)


-- type Plain = Ctor Int   (structurally portable)
plainUnions :: Map.Map Name.Name Can.Union
plainUnions =
  Map.fromList [ ("Plain", union [] [int]) ]

-- type Order = LT | EQ | GT   (allow-listed, lives in Basics)
orderUnions :: Map.Map Name.Name Can.Union
orderUnions =
  Map.fromList [ ("Order", union [] []) ]

importedAtom :: Portable.Atom
importedAtom =
  (otherHome, "Imported")


computeCases :: [(String, Bool)]
computeCases =
  [ ( "non-kernel module: a portable union is not recorded"
    , not (Set.member (home, "Tree") (nonportablesOf Set.empty False home treeUnions))
    )
  , ( "non-kernel module: a function-wrapping union is recorded"
    , Set.member (home, "Weird") (nonportablesOf Set.empty False home weirdUnions)
    )
  , ( "kernel module: even a structurally-portable union is recorded"
    , Set.member (home, "Plain") (nonportablesOf Set.empty True home plainUnions)
    )
  , ( "kernel module: an allow-listed type is not recorded"
    , not (Set.member (ModuleName.basics, "Order") (nonportablesOf Set.empty True ModuleName.basics orderUnions))
    )
  , ( "imported verdicts are carried through"
    , Set.member importedAtom (nonportablesOf (Set.singleton importedAtom) False home noUnions)
    )
  ]



-- BOUNDARY CHECK (Nitpick.Workers)


prog :: Can.Type -> Can.Type -> Can.Type -> Can.Annotation
prog args toParent msg =
  Can.Forall Map.empty (Can.TType ModuleName.workers "Program" [args, toParent, msg, int])


runBoundary :: [(A.Region, Name.Name, Can.Annotation)] -> [Maybe (Workers.BoundaryParam, Portable.Problem)]
runBoundary values =
  map classify (Workers.boundaryProblems noInfo home noUnions values)


classify :: Workers.Error -> Maybe (Workers.BoundaryParam, Portable.Problem)
classify err =
  case err of
    Workers.NonPortableBoundary _ _ param problem -> Just (param, problem)
    _                                             -> Nothing


boundaryCases :: [(String, Bool)]
boundaryCases =
  [ ( "a portable worker program has no boundary errors"
    , runBoundary [ (A.zero, "main", prog int string int) ] == []
    )
  , ( "a function message type is reported"
    , runBoundary [ (A.zero, "main", prog int int lambda) ]
        == [ Just (Workers.MsgParam, Portable.PFunction) ]
    )
  , ( "a non-portable args and msg are both reported, args first"
    , runBoundary [ (A.zero, "main", prog lambda int lambda) ]
        == [ Just (Workers.ArgsParam, Portable.PFunction), Just (Workers.MsgParam, Portable.PFunction) ]
    )
  , ( "a value that is not a worker program is ignored"
    , runBoundary [ (A.zero, "helper", Can.Forall Map.empty int) ] == []
    )
  , ( "a type variable in a boundary type is reported"
    , runBoundary [ (A.zero, "main", prog (Can.TVar "a") int int) ]
        == [ Just (Workers.ArgsParam, Portable.PTypeVar "a") ]
    )
  ]
