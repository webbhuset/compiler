module AST.Source
  ( Expr, Expr_(..), VarType(..)
  , Def(..)
  , Pattern, Pattern_(..)
  , Type, Type_(..)
  , Module(..)
  , getName
  , getImportName
  , Import(..)
  , Value(..)
  , Overload(..)
  , Signature(..)
  , Constraint(..)
  , Union(..)
  , TagDecl(..)
  , TagEntry(..)
  , Alias(..)
  , Infix(..)
  , Port(..)
  , Effects(..)
  , Manager(..)
  , Docs(..)
  , Comment(..)
  , Exposing(..)
  , Exposed(..)
  , Privacy(..)
  )
  where


import Data.Name (Name)
import qualified Data.Name as Name

import qualified AST.Utils.Binop as Binop
import qualified AST.Utils.Css as Css
import qualified AST.Utils.Shader as Shader
import qualified Elm.Float as EF
import qualified Elm.String as ES
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A



-- EXPRESSIONS


type Expr = A.Located Expr_


data Expr_
  = Chr Char
  | Str ES.String
  | Int Integer
  | Float EF.Float
  | Var VarType Name
  | VarQual VarType Name Name
  | List [Expr]
  | Op Name
  | Negate Expr
  | Binops [(Expr, A.Located Name)] Expr
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [A.Located Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Accessor Name
  | Access Expr (A.Located Name)
  | Update (A.Located Name) [(A.Located Name, Expr)]
  | Record [(A.Located Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | Shader Shader.Source Shader.Types
  | Css Css.Content


data VarType = LowVar | CapVar



-- DEFINITIONS


data Def
  = Define (A.Located Name) [Pattern] Expr (Maybe Signature)
  | Destruct Pattern Expr



-- PATTERN


type Pattern = A.Located Pattern_


data Pattern_
  = PAnything
  | PVar Name
  | PRecord [A.Located Name]
  | PAlias Pattern (A.Located Name)
  | PUnit
  | PTuple Pattern Pattern [Pattern]
  | PCtor A.Region Name [Pattern]
  | PCtorQual A.Region Name Name [Pattern]
  | PList [Pattern]
  | PCons Pattern Pattern
  | PChr Char
  | PStr ES.String
  | PInt Integer



-- TYPE


type Type =
    A.Located Type_


data Type_
  = TLambda Type Type
  | TVar Name
  | TType A.Region Name [Type]
  | TTypeQual A.Region Name Name [Type]
  | TRecord [(A.Located Name, Type)] (Maybe (A.Located Name))
  | TUnit
  | TTuple Type Type [Type]
  | TTagRow [TagEntry] (Maybe (A.Located Name))


data TagEntry =
  TagEntry A.Region (Maybe Name) Name [Type]



-- MODULE


data Module =
  Module
    { _name    :: Maybe (A.Located Name)
    , _exports :: A.Located Exposing
    , _docs    :: Docs
    , _imports :: [Import]
    , _values  :: [A.Located Value]
    , _unions  :: [A.Located Union]
    , _aliases :: [A.Located Alias]
    , _tagDecls :: [A.Located TagDecl]
    , _overloads :: [A.Located Overload]
    , _binops  :: [A.Located Infix]
    , _effects :: Effects
    }


getName :: Module -> Name
getName (Module maybeName _ _ _ _ _ _ _ _ _ _) =
  case maybeName of
    Just (A.At _ name) ->
      name

    Nothing ->
      Name._Main


getImportName :: Import -> Name
getImportName (Import (A.At _ name) _ _) =
  name


data Import =
  Import
    { _import :: A.Located Name
    , _alias :: Maybe Name
    , _exposing :: Exposing
    }


data Value = Value (A.Located Name) [Pattern] Expr (Maybe Signature)


-- A type annotation together with the overloads it needs, written under it:
--
--     sort : List a -> List a
--         where Ord.compare : a -> a -> Ordering
--
data Signature =
  Signature Type [A.Located Constraint]


-- One `where` line: an overloaded name and the type this signature needs it
-- at. The type variable it dispatches on is one of the enclosing signature's.
data Constraint =
  Constraint (A.Located Name) (A.Located Name) Type


-- An overload: a value written with a QUALIFIED name, which is what marks
-- it as one. With no body it declares an abstract name that other modules
-- define; with a body it defines that name for one type.
--
--     Order.compare : a -> a -> Order              -- in module Order
--     Order.compare : Card -> Card -> Order        -- in module Card
--     Order.compare a b = ...
--
data Overload =
  Overload
    { _ov_qual :: A.Located Name      -- the module part, as written
    , _ov_name :: A.Located Name      -- the value part
    , _ov_type :: Signature
    , _ov_body :: Maybe ([Pattern], Expr)
    }
data Union = Union (A.Located Name) [A.Located Name] [(A.Located Name, [Type])]
data TagDecl = TagDecl (A.Located Name) [A.Located Name]
data Alias = Alias (A.Located Name) [A.Located Name] Type
data Infix = Infix Name Binop.Associativity Binop.Precedence Name
data Port = Port (A.Located Name) Type


data Effects
  = NoEffects
  | Ports [Port]
  | Manager A.Region Manager


data Manager
  = Cmd (A.Located Name)
  | Sub (A.Located Name)
  | Fx (A.Located Name) (A.Located Name)


data Docs
  = NoDocs A.Region
  | YesDocs Comment [(Name, Comment)]


newtype Comment =
  Comment P.Snippet



-- EXPOSING


data Exposing
  = Open
  | Explicit [Exposed]


data Exposed
  = Lower (A.Located Name)
  | Upper (A.Located Name) Privacy
  | Operator A.Region Name


data Privacy
  = Public A.Region
  | Private
