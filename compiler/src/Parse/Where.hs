{-# LANGUAGE ExtendedLiterals, MagicHash, OverloadedStrings #-}
module Parse.Where
  ( clauses
  )
  where


import Data.Name (Name)

import qualified AST.Source as Src
import qualified Parse.Keyword as Keyword
import qualified Parse.Space as Space
import qualified Parse.Type as Type
import qualified Parse.Variable as Var
import Parse.Primitives hiding (State)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- WHERE CLAUSES
--
-- The overloads a signature needs, written under it:
--
--     sort : List a -> List a
--         where Ord.compare : a -> a -> Ordering
--
-- Each clause is indented under the signature, so finding out whether there is
-- one means reading past the end of the signature. That lookahead is undone
-- when the next line turns out to be the body instead.


clauses :: A.Position -> Space.Parser E.Where [A.Located Src.Constraint]
clauses end =
  clausesHelp end []


clausesHelp :: A.Position -> [A.Located Src.Constraint] -> Space.Parser E.Where [A.Located Src.Constraint]
clausesHelp end revClauses =
  oneOfWithFallback
    [ do  start <-
            backtrack $
              do  Space.chompAndCheckIndent E.WhereSpace E.WhereIndentWhere
                  position <- getPosition
                  Keyword.where_ E.WhereIndentWhere
                  return position

          Space.chompAndCheckIndent E.WhereSpace E.WhereIndentName
          qualified <- addLocation (Var.foreignAlpha E.WhereName)
          (qual, name) <- toConstraintName qualified
          Space.chompAndCheckIndent E.WhereSpace E.WhereIndentColon
          word1 0x3A#Word8 {-:-} E.WhereColon
          Space.chompAndCheckIndent E.WhereSpace E.WhereIndentType
          (tipe, clauseEnd) <- specialize (E.WhereType (A.toValue name)) Type.expression
          clausesHelp clauseEnd (A.at start clauseEnd (Src.Constraint qual name tipe) : revClauses)
    ]
    ( reverse revClauses, end )


-- A constraint names an overload, which is always qualified.
toConstraintName :: A.Located Src.Expr_ -> Parser E.Where (A.Located Name, A.Located Name)
toConstraintName (A.At region expr) =
  case expr of
    Src.VarQual Src.LowVar qual name ->
      return (A.At region qual, A.At region name)

    _ ->
      P.Parser $ \_ (P.State _ _ _ cur) _ _ _ eerr -> eerr cur E.WhereName
