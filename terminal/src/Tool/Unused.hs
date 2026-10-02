module Tool.Unused
  ( run
  )
  where


import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Build
import qualified Elm.Interface as I
import qualified Json.Encode as E
import qualified Reporting.Annotation as A
import Tool.Index (Ref(..))
import qualified Tool.Index as Index
import Tool.Output (Output(..))
import Tool.Place (Place(..))
import qualified Tool.Place as Place
import Tool.Problem (Problem)
import qualified Tool.Project as Project



-- RUN
--
--   elm tool unused
--
-- What nothing in the project uses: top level values, constructors that are
-- never built, imports, names in `exposing` lists of imports, and modules
-- that no `main` reaches. In a package, what it exposes is its API and does
-- not count as unused.
--
-- Two definitions that only use each other both look used.


run :: IO (Either Problem Output)
run =
  Project.withProject $ \root exposedModules modules ->
    do  let indexed = Index.index modules
            found = Place.sortPlaces (concatMap (findIn exposedModules indexed) indexed)
                    ++ unusedModules exposedModules modules
        places <- Place.withSource root found
        return $ Right $ Output
          (if null places then "Nothing unused.\n" else concatMap Place.toText places)
          (E.list Place.toJson places)



-- FIND


findIn :: Maybe [Module.Name] -> [Index.Module] -> Index.Module -> [Place]
findIn exposedModules everything (Index.Module path checked refs) =
  let
    src = Build._checked_source checked
    can = Build._checked_canonical checked
    home = Src.getName src
    isApi name = maybe False (elem home) exposedModules && exposedIn can name

    allRefs =
      [ (Src.getName (Build._checked_source (Index._checked m)), r) | m <- everything, r <- Index._refs m ]

    -- uses of a name, outside its own definition
    usedBy kinds target =
      [ r | (from, r) <- allRefs, _target r == target, elem (_kind r) kinds, not (isSelf target from r) ]

    isSelf (m, n) from r = m == from && _in r == Just n

    uses = [Index.Value, Index.Construct, Index.Match, Index.Type, Index.Operator]

    place region note = (Place.fromRegion path note region) { _note = note }

    values =
      [ place r "unused"
      | Ref _ r Index.Definition (_, n) <- refs
      , isValue src n
      , n /= "main"
      , not (isPort src n)
      , not (isApi n)
      , null (usedBy uses (home, n))
      ]

    ctors =
      [ place r (if null (usedBy [Index.Match] (home, n)) then "unused constructor" else "never constructed")
      | Ref (Just typeName) r Index.Definition (_, n) <- refs
      , isCtor can typeName n
      , not (isApi typeName)
      , null (usedBy [Index.Construct] (home, n))
      ]

    types =
      [ place r "unused type"
      | Ref _ r Index.Definition (_, n) <- refs
      , isType can n
      , not (isApi n)
      , null (usedBy uses (home, n))
      ]

    usedHere = Set.fromList [ fst (_target r) | r <- refs, elem (_kind r) uses ]
    -- names used without a qualifier, which is what exposing them is for
    usedNamesHere = Set.fromList [ _target r | r <- refs, elem (_kind r) uses, unqualified r ]

    imports =
      [ place r "unused import"
      | Src.Import (A.At r m) _ _ _ <- Src._imports src
      , not (isImplicit r)
      , Set.notMember m usedHere
      ]

    exposings =
      [ place r "unused in this module"
      | Src.Import (A.At ir m) _ (Src.Explicit exposed) _ <- Src._imports src
      , not (isImplicit ir)
      , Set.member m usedHere
      , (r, n) <- [ (r, N.toChars n) | Src.Lower (A.At r n) <- exposed ] ++ [ (r, T.nameToChars n) | Src.Upper (A.At r n) _ <- exposed ]
      , Set.notMember (m, n) usedNamesHere
      , not (any (\c -> Set.member (m, c) usedNamesHere) (ctorsOfImported checked m n))
      ]
  in
  values ++ ctors ++ types ++ imports ++ exposings


isValue :: Src.Module -> String -> Bool
isValue src n =
  or [ N.toChars v == n | A.At _ (Src.Value (A.At _ v) _ _ _) <- Src._values src ]


isPort :: Src.Module -> String -> Bool
isPort src n =
  case Src._effects src of
    Src.Ports ports -> or [ N.toChars p == n | Src.Port (A.At _ p) _ <- ports ]
    _               -> False


isType :: Can.Module -> String -> Bool
isType can n =
  any ((== n) . T.nameToChars) (Map.keys (Can._unions can) ++ Map.keys (Can._aliases can))


isCtor :: Can.Module -> String -> String -> Bool
isCtor can typeName n =
  or [ N.toChars c == n
     | (t, u) <- Map.toList (Can._unions can), T.nameToChars t == typeName
     , Can.Ctor c _ _ _ <- Can._u_alts u
     ]


exposedIn :: Can.Module -> String -> Bool
exposedIn can n =
  case Can._exports can of
    Can.ExportEverything _ -> True
    Can.Export types values _ ->
      any ((== n) . T.nameToChars) (Map.keys types) || any ((== n) . N.toChars) (Map.keys values)


-- `import M exposing (Msg(..))` is used when a constructor of Msg is.
ctorsOfImported :: Build.Checked -> Module.Name -> String -> [String]
ctorsOfImported checked m n =
  case Map.lookup m (Build._checked_interfaces checked) of
    Nothing -> []
    Just iface ->
      [ N.toChars c
      | (t, iu) <- Map.toList (I._unions iface)
      , T.nameToChars t == n
      , Just u <- [Index.publicUnion iu]
      , Can.Ctor c _ _ _ <- Can._u_alts u
      ]


-- A reference written without a module in front: its region is exactly as
-- wide as the name. Patterns cover their arguments too, so they count when
-- they start with the name.
unqualified :: Ref -> Bool
unqualified (Ref _ region kind (_, n)) =
  let
    ((sr, sc), (er, ec)) = (Index.start region, Index.end region)
  in
  case kind of
    Index.Match    -> True
    Index.Operator -> True
    _              -> sr == er && ec - sc == length n


isImplicit :: A.Region -> Bool
isImplicit region =
  Index.start region == Index.end region



-- MODULES


-- Modules that no module with a `main`, or no exposed module of a package,
-- imports, directly or not.
unusedModules :: Maybe [Module.Name] -> [(FilePath, Build.Checked)] -> [Place]
unusedModules exposedModules modules =
  let
    byName = Map.fromList [ (Src.getName (Build._checked_source c), (path, c)) | (path, c) <- modules ]
    importsOf m = maybe [] (\(_, c) -> map Src.getImportName (Src._imports (Build._checked_source c))) (Map.lookup m byName)
    roots =
      case exposedModules of
        Just exposed -> exposed
        Nothing -> [ m | (m, (_, c)) <- Map.toList byName, any ((== "main") . N.toChars) (Map.keys (Build._checked_annotations c)) ]
    reached = go Set.empty roots
    go seen todo =
      case todo of
        [] -> seen
        m : rest
          | Set.member m seen -> go seen rest
          | otherwise -> go (Set.insert m seen) (importsOf m ++ rest)
  in
  if null roots then [] else
    [ (Place.fromRegion path "unused module" A.zero) { _note = "unused module", _line = 1, _column = 1 }
    | (m, (path, _)) <- Map.toList byName
    , Set.notMember m reached
    ]

