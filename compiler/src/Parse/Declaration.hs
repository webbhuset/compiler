{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, OverloadedStrings #-}
module Parse.Declaration
  ( Decl(..)
  , declaration
  , infix_
  )
  where


import qualified Data.Name as Name
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.Word (Word8(..))

import qualified AST.Source as Src
import qualified AST.Utils.Binop as Binop
import qualified Parse.Expression as Expr
import qualified Parse.Pattern as Pattern
import qualified Parse.Keyword as Keyword
import qualified Parse.Space as Space
import qualified Parse.Symbol as Symbol
import qualified Parse.Type as Type
import qualified Parse.Variable as Var
import Parse.Primitives hiding (State)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- DECLARATION


data Decl
  = Value (Maybe Src.Comment) (A.Located Src.Value)
  | Overload (Maybe Src.Comment) (A.Located Src.Overload)
  | Union (Maybe Src.Comment) (A.Located Src.Union)
  | Alias (Maybe Src.Comment) (A.Located Src.Alias)
  | TagDecl (Maybe Src.Comment) (A.Located Src.TagDecl)
  | Port (Maybe Src.Comment) Src.Port


declaration :: Space.Parser E.Decl Decl
declaration =
  do  maybeDocs <- chompDocComment
      start <- getPosition
      oneOf E.DeclStart
        [ typeDecl maybeDocs start
        , portDecl maybeDocs
        , valueDecl maybeDocs start
        , overloadDecl maybeDocs start
        ]



-- DOC COMMENT


chompDocComment :: Parser E.Decl (Maybe Src.Comment)
chompDocComment =
  oneOfWithFallback
    [ do  comment <- Space.docComment E.DeclStart E.DeclSpace
          Space.chomp E.DeclSpace
          Space.checkFreshLine E.DeclFreshLineAfterDocComment
          return (Just comment)
    ]
    Nothing



-- DEFINITION


{-# INLINE valueDecl #-}
valueDecl :: Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Decl
valueDecl maybeDocs start =
  do  name <- Var.lower E.DeclStart
      end <- getPosition
      specialize (E.DeclDef name) $
        do  Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
            oneOf E.DeclDefEquals
              [
                do  word1 0x3A#Word8 {-:-} E.DeclDefEquals
                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentType
                    (tipe, _) <- specialize E.DeclDefType Type.expression
                    Space.checkFreshLine E.DeclDefNameRepeat
                    defName <- chompMatchingName name
                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                    chompDefArgsAndBody maybeDocs start defName (Just tipe) []
              ,
                chompDefArgsAndBody maybeDocs start (A.at start end name) Nothing []
              ]



-- OVERLOADS
--
-- A declaration whose name is QUALIFIED is an overload:
--
--     Order.compare : a -> a -> Order              -- abstract, no body
--
--     Order.compare : Card -> Card -> Order        -- a definition
--     Order.compare a b = ...
--
-- A qualified name has never been legal at the start of a declaration, and
-- valueDecl gives up without consuming when it sees an upper case letter,
-- so nothing that used to compile changes meaning.


{-# INLINE overloadDecl #-}
overloadDecl :: Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Decl
overloadDecl maybeDocs start =
  do  qualified <- addLocation (Var.foreignAlpha E.DeclStart)
      (qual, name) <- toOverloadName qualified
      specialize (E.DeclOverload (A.toValue name)) $
        do  Space.chompAndCheckIndent E.OverloadSpace E.OverloadIndentColon
            word1 0x3A#Word8 {-:-} E.OverloadColon
            Space.chompAndCheckIndent E.OverloadSpace E.OverloadIndentType
            (tipe, typeEnd) <- specialize E.OverloadType Type.expression
            oneOfWithFallback
              [ do  bodyName <-
                      backtrack $
                        do  Space.checkFreshLine E.OverloadIndentBody
                            defName <- addLocation (Var.foreignAlpha E.OverloadBodyName)
                            n <- matchOverloadName qual name defName
                            Space.chompAndCheckIndent E.OverloadSpace E.OverloadIndentBody
                            notColon
                            return n

                    ((args, body), end) <- chompOverloadBody []
                    let ov = Src.Overload qual bodyName tipe (Just (args, body))
                    return (Overload maybeDocs (A.at start end ov), end)
              ]
              ( Overload maybeDocs (A.at start typeEnd (Src.Overload qual name tipe Nothing))
              , typeEnd
              )


-- Only `Module.name` counts; a bare name is an ordinary definition and an
-- upper case one is something else entirely.
toOverloadName :: A.Located Src.Expr_ -> Parser E.Decl (A.Located Name.Name, A.Located Name.Name)
toOverloadName (A.At region expr) =
  case expr of
    Src.VarQual Src.LowVar home name ->
      return (A.At region home, A.At region name)

    _ ->
      P.Parser $ \_ (P.State _ _ _ cur) _ _ _ eerr -> eerr cur E.DeclStart


-- The line after a signature only belongs to it if it repeats the same
-- qualified name. Anything else is the next declaration, and failing here
-- without committing is what lets the signature stand on its own as an
-- abstract declaration.
-- Deciding whether a signature has a body means reading past the end of the
-- signature, so the lookahead has to be undoable: without this a failure part
-- way through would be a syntax error instead of "there is no body here".
backtrack :: Parser x a -> Parser x a
backtrack (Parser parser) =
  P.Parser $ \fpc state@(P.State _ _ _ start) cok eok _ eerr ->
    parser fpc state cok eok (\_ toError -> eerr start toError) eerr


-- The same name again followed by a colon is the next declaration, not this
-- one's body: an abstract declaration and a definition of it can sit next to
-- each other in the module that owns the name.
notColon :: Parser E.Overload ()
notColon =
  P.Parser $ \_ state@(P.State pos end _ cur) _ eok _ eerr ->
    if ltAddr pos end && eqIndex pos 0# 0x3A#Word8 {-:-} then
      eerr cur E.OverloadBodyName
    else
      eok () state


matchOverloadName :: A.Located Name.Name -> A.Located Name.Name -> A.Located Src.Expr_ -> Parser E.Overload (A.Located Name.Name)
matchOverloadName (A.At _ qual) (A.At _ name) (A.At region expr) =
  case expr of
    Src.VarQual Src.LowVar bodyQual bodyName | bodyQual == qual && bodyName == name ->
      return (A.At region bodyName)

    _ ->
      P.Parser $ \_ (P.State _ _ _ cur) _ _ _ eerr -> eerr cur E.OverloadBodyName


chompOverloadBody :: [Src.Pattern] -> Space.Parser E.Overload ([Src.Pattern], Src.Expr)
chompOverloadBody revArgs =
  oneOf E.OverloadEquals
    [ do  arg <- specialize E.OverloadArg Pattern.term
          Space.chompAndCheckIndent E.OverloadSpace E.OverloadIndentBody
          chompOverloadBody (arg:revArgs)
    , do  word1 0x3D#Word8 {-=-} E.OverloadEquals
          Space.chompAndCheckIndent E.OverloadSpace E.OverloadIndentBody
          (body, end) <- specialize E.OverloadBody Expr.expression
          return ((reverse revArgs, body), end)
    ]



chompDefArgsAndBody :: Maybe Src.Comment -> A.Position -> A.Located Name.Name -> Maybe Src.Type -> [Src.Pattern] -> Space.Parser E.DeclDef Decl
chompDefArgsAndBody maybeDocs start name tipe revArgs =
  oneOf E.DeclDefEquals
    [ do  arg <- specialize E.DeclDefArg Pattern.term
          Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
          chompDefArgsAndBody maybeDocs start name tipe (arg : revArgs)
    , do  word1 0x3D#Word8 {-=-} E.DeclDefEquals
          Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentBody
          (body, end) <- specialize E.DeclDefBody Expr.expression
          let value = Src.Value name (reverse revArgs) body tipe
          let avalue = A.at start end value
          return (Value maybeDocs avalue, end)
    ]


chompMatchingName :: Name.Name -> Parser E.DeclDef (A.Located Name.Name)
chompMatchingName expectedName =
  let
    (P.Parser k) = Var.lower E.DeclDefNameRepeat
  in
  P.Parser $ \fpc state@(P.State _ _ _ start) cok eok cerr eerr ->
    let
      cokL name newState@(P.State _ _ _ end) =
        if expectedName == name
        then cok (A.At (A.Region start end) name) newState
        else cerr start (E.DeclDefNameMatch name)

      eokL name newState@(P.State _ _ _ end) =
        if expectedName == name
        then eok (A.At (A.Region start end) name) newState
        else eerr start (E.DeclDefNameMatch name)
    in
    k fpc state cokL eokL cerr eerr



-- TYPE DECLARATIONS


{-# INLINE typeDecl #-}
typeDecl :: Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Decl
typeDecl maybeDocs start =
  inContext E.DeclType (Keyword.type_ E.DeclStart) $
    do  Space.chompAndCheckIndent E.DT_Space E.DT_IndentName
        oneOf E.DT_Name
          [
            inContext E.DT_Alias (Keyword.alias_ E.DT_Name) $
              do  Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                  (name, args) <- chompAliasNameToEquals
                  (tipe, end) <- specialize E.AliasBody Type.expression
                  let alias = A.at start end (Src.Alias name args tipe)
                  return (Alias maybeDocs alias, end)
          ,
            inContext E.DT_Tag (Keyword.tag_ E.DT_Name) $
              do  Space.chompAndCheckIndent E.DT_TagSpace E.DT_TagIndentName
                  name <- addLocation (Var.upper E.DT_TagName)
                  nameEnd <- getPosition
                  Space.chomp E.DT_TagSpace
                  (args, end) <- chompTagArgs [] nameEnd
                  let tag = A.at start end (Src.TagDecl name args)
                  return (TagDecl maybeDocs tag, end)
          ,
            specialize E.DT_Union $
              do  (name, args) <- chompCustomNameToEquals
                  (firstVariant, firstEnd) <- Type.variant
                  (variants, end) <- chompVariants [firstVariant] firstEnd
                  let union = A.at start end (Src.Union name args variants)
                  return (Union maybeDocs union, end)
          ]



-- TYPE ALIASES


chompAliasNameToEquals :: Parser E.TypeAlias (A.Located Name.Name, [A.Located Name.Name])
chompAliasNameToEquals =
  do  name <- addLocation (Var.upper E.AliasName)
      Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
      chompAliasNameToEqualsHelp name []


chompAliasNameToEqualsHelp :: A.Located Name.Name -> [A.Located Name.Name] -> Parser E.TypeAlias (A.Located Name.Name, [A.Located Name.Name])
chompAliasNameToEqualsHelp name args =
  oneOf E.AliasEquals
    [ do  arg <- addLocation (Var.lower E.AliasEquals)
          Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
          chompAliasNameToEqualsHelp name (arg:args)
    , do  word1 0x3D#Word8 {-=-} E.AliasEquals
          Space.chompAndCheckIndent E.AliasSpace E.AliasIndentBody
          return ( name, reverse args )
    ]



-- CUSTOM TYPES


chompCustomNameToEquals :: Parser E.CustomType (A.Located Name.Name, [A.Located Name.Name])
chompCustomNameToEquals =
  do  name <- addLocation (Var.upper E.CT_Name)
      Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
      chompCustomNameToEqualsHelp name []


chompCustomNameToEqualsHelp :: A.Located Name.Name -> [A.Located Name.Name] -> Parser E.CustomType (A.Located Name.Name, [A.Located Name.Name])
chompCustomNameToEqualsHelp name args =
  oneOf E.CT_Equals
    [ do  arg <- addLocation (Var.lower E.CT_Equals)
          Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
          chompCustomNameToEqualsHelp name (arg:args)
    , do  word1 0x3D#Word8 {-=-} E.CT_Equals
          Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterEquals
          return ( name, reverse args )
    ]


chompVariants :: [(A.Located Name.Name, [Src.Type])] -> A.Position -> Space.Parser E.CustomType [(A.Located Name.Name, [Src.Type])]
chompVariants variants end =
  oneOfWithFallback
    [ do  Space.checkIndent end E.CT_IndentBar
          word1 0x7C#Word8 {-|-} E.CT_Bar
          Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterBar
          (variant, newEnd) <- Type.variant
          chompVariants (variant:variants) newEnd
    ]
    (reverse variants, end)



-- STRUCTURAL VARIANT TAGS


chompTagArgs :: [A.Located Name.Name] -> A.Position -> Space.Parser E.DeclTag [A.Located Name.Name]
chompTagArgs revArgs end =
  oneOfWithFallback
    [ do  Space.checkIndent end E.DT_TagIndentArg
          arg <- addLocation (Var.lower E.DT_TagArg)
          newEnd <- getPosition
          Space.chomp E.DT_TagSpace
          chompTagArgs (arg:revArgs) newEnd
    , -- an uppercase name or a paren here means someone wrote a payload
      -- TYPE; commit to a dedicated error instead of ending the
      -- declaration and letting the top level trip on the leftovers
      do  Space.checkIndent end E.DT_TagIndentArg
          A.Position cur <- getPosition
          oneOf E.DT_TagArg
            [ do  _ <- Var.upper E.DT_TagArg
                  return ()
            , word1 0x28#Word8 {-(-} E.DT_TagArg
            ]
          P.Parser $ \_ _ _ _ cerr _ -> cerr cur E.DT_TagArgType
    ]
    (reverse revArgs, end)



-- PORT


{-# INLINE portDecl #-}
portDecl :: Maybe Src.Comment -> Space.Parser E.Decl Decl
portDecl maybeDocs =
  inContext E.Port (Keyword.port_ E.DeclStart) $
    do  Space.chompAndCheckIndent E.PortSpace E.PortIndentName
        name <- addLocation (Var.lower E.PortName)
        Space.chompAndCheckIndent E.PortSpace E.PortIndentColon
        word1 0x3A#Word8 {-:-} E.PortColon
        Space.chompAndCheckIndent E.PortSpace E.PortIndentType
        (tipe, end) <- specialize E.PortType Type.expression
        return
          ( Port maybeDocs (Src.Port name tipe)
          , end
          )



-- INFIX


-- INVARIANT: always chomps to a freshline
--
infix_ :: Parser E.Module (A.Located Src.Infix)
infix_ =
  let
    err = E.Infix
    _err = \_ -> E.Infix
  in
  do  start <- getPosition
      Keyword.infix_ err
      Space.chompAndCheckIndent _err err
      assoc <-
        oneOf err
          [ Keyword.left_  err >> return Binop.Left
          , Keyword.right_ err >> return Binop.Right
          , Keyword.non_   err >> return Binop.Non
          ]
      Space.chompAndCheckIndent _err err
      prec <- precedence err
      Space.chompAndCheckIndent _err err
      word1 0x28#Word8 {-(-} err
      op <- Symbol.operator err _err
      word1 0x29#Word8 {-)-} err
      Space.chompAndCheckIndent _err err
      word1 0x3D#Word8 {-=-} err
      Space.chompAndCheckIndent _err err
      name <- Var.lower err
      end <- getPosition
      Space.chomp _err
      Space.checkFreshLine err
      return (A.at start end (Src.Infix op assoc prec name))


precedence :: (Cursor -> x) -> Parser x Binop.Precedence
precedence toExpectation =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    if P.notLtAddr pos end then
      eerr cur toExpectation

    else
      let !word = indexWord8OffAddr# pos 0# in
      if isDecDigit word then
        cok
          (Binop.Precedence (fromIntegral (W8# word - 0x30 {-0-})))
          (P.State (plusAddr# pos 1#) end indent (P.slide cur 1#Word64))

      else
        eerr cur toExpectation


{-# INLINE isDecDigit #-}
isDecDigit :: Word8# -> Bool
isDecDigit word =
  isTrue# (leWord8# 0x30#Word8 word) && isTrue# (leWord8# word 0x39#Word8)
