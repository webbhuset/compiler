{-# LANGUAGE OverloadedStrings #-}
module Tool.Cases
  ( run
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Build
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import Tool.Index (Ref(..))
import qualified Tool.Index as Index
import Tool.Output (Output(..))
import Tool.Place (Place(..))
import qualified Tool.Place as Place
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import qualified Tool.Refs as Refs
import qualified Tool.Summary as Summary



-- RUN
--
--   elm tool cases Some.Module.Type
--
-- Every `case` on a custom type, with the constructors it names and the
-- ones a wildcard branch covers without naming them, which are the places
-- the compiler will not point at when a constructor is added. Then every
-- place a constructor of the type is built.
--
--   src/Main.elm:31:5: case msg of  -- Increment, Decrement; _ covers Named


run :: String -> IO (Either Problem Output)
run target =
  case Refs.parseTarget target of
    Nothing ->
      return (Left (BadName "a type like `Page.Home.Msg`" target))

    Just (home, typeName) ->
      Project.withProject $ \root _ modules ->
        case ctorsOf home typeName modules of
          Nothing ->
            return $ Left $ NotFound ("a custom type in " ++ Module.toChars home) typeName $
              Summary.suggest typeName (typesIn home modules)

          Just ctors ->
            do  let matches = Place.sortPlaces (concatMap (casesIn home typeName ctors) modules)
                    builds =
                      Place.sortPlaces
                        [ (Place.fromRegion (Index._path m) "built" (_region r)) { _note = "builds " ++ snd (_target r) }
                        | m <- Index.index modules
                        , r <- Index._refs m
                        , _kind r == Index.Construct
                        , fst (_target r) == home
                        , elem (snd (_target r)) ctors
                        ]
                matched <- Place.withSource root matches
                built <- Place.withSource root builds
                return $ Right $ Output
                  ( section "Matched:" matched ++ (if null matched || null built then "" else "\n") ++ section "Built:" built )
                  ( E.object [ "cases" ==> E.list Place.toJson matched, "built" ==> E.list Place.toJson built ] )


section :: String -> [Place] -> String
section title places =
  if null places then "" else title ++ "\n" ++ concatMap Place.toText places



-- CONSTRUCTORS


ctorsOf :: Module.Name -> String -> [(FilePath, Build.Checked)] -> Maybe [String]
ctorsOf home typeName modules =
  let
    fromUnion u = [ N.toChars c | Can.Ctor c _ _ _ <- Can._u_alts u ]
    local =
      [ fromUnion u
      | (_, c) <- modules
      , ModuleName._module (Can._name (Build._checked_canonical c)) == home
      , (t, u) <- Map.toList (Can._unions (Build._checked_canonical c))
      , T.nameToChars t == typeName
      ]
    foreign =
      [ fromUnion u
      | (_, c) <- modules
      , Just iface <- [Map.lookup home (Build._checked_interfaces c)]
      , (t, iu) <- Map.toList (I._unions iface)
      , T.nameToChars t == typeName
      , Just u <- [Index.publicUnion iu]
      ]
  in
  case local ++ foreign of
    cs : _ -> Just cs
    []     -> Nothing


typesIn :: Module.Name -> [(FilePath, Build.Checked)] -> [String]
typesIn home modules =
  List.nub
    [ T.nameToChars t
    | (_, c) <- modules
    , ModuleName._module (Can._name (Build._checked_canonical c)) == home
    , t <- Map.keys (Can._unions (Build._checked_canonical c))
    ]



-- CASES


casesIn :: Module.Name -> String -> [String] -> (FilePath, Build.Checked) -> [Place]
casesIn home typeName ctors (path, checked) =
  let
    isOurs h t = ModuleName._module h == home && T.nameToChars t == typeName

    -- the constructors of the type a pattern names, anywhere in it
    named (A.At _ p) =
      case p of
        Can.PCtor h t _ c _ args -> [ N.toChars c | isOurs h t ] ++ concatMap (named . Can._arg) args
        Can.PAlias a _           -> named a
        Can.PPair a b            -> named a ++ named b
        Can.PTriple a b c        -> named a ++ named b ++ named c
        Can.PList ps             -> concatMap named ps
        Can.PCons a b            -> named a ++ named b
        Can.PTag _ _ _ args      -> concatMap named args
        _                        -> []

    toPlace region branches =
      let
        names = List.nub (concatMap (named . branchPattern) branches)
        wild = any (null . named . branchPattern) branches
        covered = filter (`notElem` names) ctors
        note =
          List.intercalate ", " names
          ++ ( if wild && not (null covered) then "; _ covers " ++ List.intercalate ", " covered
               else if wild then "; plus a wildcard"
               else "" )
      in
      (Place.fromRegion path (if wild && not (null covered) then "wildcard" else "case") region) { _note = note }

    found =
      [ toPlace region branches
      | (region, branches) <- concatMap exprCases (declExprs (Can._decls (Build._checked_canonical checked)))
      , any (not . null . named . branchPattern) branches
      ]
  in
  found


branchPattern :: Can.CaseBranch -> Can.Pattern
branchPattern (Can.CaseBranch p _) =
  p


declExprs :: Can.Decls -> [Can.Expr]
declExprs decls =
  case decls of
    Can.Declare def rest         -> defBody def : declExprs rest
    Can.DeclareRec def defs rest -> map defBody (def : defs) ++ declExprs rest
    Can.SaveTheEnvironment       -> []


defBody :: Can.Def -> Can.Expr
defBody def =
  case def of
    Can.Def _ _ body          -> body
    Can.TypedDef _ _ _ body _ -> body


-- Every `case` in an expression, with its branches.
exprCases :: Can.Expr -> [(A.Region, [Can.CaseBranch])]
exprCases (A.At region e) =
  let go = exprCases in
  case e of
    Can.Case subject branches -> (region, branches) : go subject ++ concat [ go b | Can.CaseBranch _ b <- branches ]
    Can.List es               -> concatMap go es
    Can.Negate a              -> go a
    Can.Widen a               -> go a
    Can.Binop _ _ _ _ a b     -> go a ++ go b
    Can.Lambda _ body         -> go body
    Can.Call f args           -> concatMap go (f : args)
    Can.If branches final     -> concat [ go c ++ go b | (c, b) <- branches ] ++ go final
    Can.Let def body          -> go (defBody def) ++ go body
    Can.LetRec defs body      -> concatMap (go . defBody) defs ++ go body
    Can.LetDestruct _ a b     -> go a ++ go b
    Can.Access a _            -> go a
    Can.Update _ a fields     -> go a ++ concat [ go f | Can.FieldUpdate _ f <- Map.elems fields ]
    Can.Record fields         -> concatMap go (Map.elems fields)
    Can.Pair a b              -> go a ++ go b
    Can.Triple a b c          -> go a ++ go b ++ go c
    _                         -> []
