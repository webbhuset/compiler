module AST.Source
  ( Expr, Expr_(..), VarType(..)
  , Def(..)
  , Pattern, Pattern_(..)
  , Type, Type_(..)
  , Module(..)
  , getName
  , getImportName
  , Import(..)
  , Loading(..)
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


import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
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
  | Var VarType N.Name
  | VarQual VarType Module.Prefix N.Name
  | List [Expr]
  | Op Op.Name
  | Negate Expr
  | Binops [(Expr, A.Located Op.Name)] Expr
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [A.Located Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Accessor N.Name
  | Access Expr (A.Located N.Name)
  | Update (A.Located N.Name) [(A.Located N.Name, Expr)]
  | Record [(A.Located N.Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | Shader Shader.Source Shader.Types
  | Css Css.Content


data VarType = LowVar | CapVar



-- DEFINITIONS


data Def
  = Define (A.Located N.Name) [Pattern] Expr (Maybe Signature)
  | Destruct Pattern Expr



-- PATTERN


type Pattern = A.Located Pattern_


data Pattern_
  = PAnything
  | PVar N.Name
  | PRecord [A.Located N.Name]
  | PAlias Pattern (A.Located N.Name)
  | PUnit
  | PTuple Pattern Pattern [Pattern]
  | PCtor A.Region N.Name [Pattern]
  | PCtorQual A.Region Module.Prefix N.Name [Pattern]
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
  | TVar T.Var
  | TType A.Region T.Name [Type]
  | TTypeQual A.Region Module.Prefix T.Name [Type]
  | TRecord [(A.Located N.Name, Type)] (Maybe (A.Located T.Var))
  | TUnit
  | TTuple Type Type [Type]
  | TTagRow [TagEntry] (Maybe (A.Located T.Var))


data TagEntry =
  TagEntry A.Region (Maybe Module.Prefix) N.Name [Type]



-- MODULE


data Module =
  Module
    { _name    :: Maybe (A.Located Module.Name)
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


getName :: Module -> Module.Name
getName (Module maybeName _ _ _ _ _ _ _ _ _ _) =
  case maybeName of
    Just (A.At _ name) -> name
    Nothing            -> Module.main


getImportName :: Import -> Module.Name
getImportName (Import (A.At _ name) _ _ _) =
  name


data Import =
  Import
    { _import :: A.Located Module.Name
    , _alias :: Maybe Module.Prefix
    , _exposing :: Exposing
    , _loading :: Loading
    }


-- `import async M` says the module's code may arrive in a separate file,
-- fetched the first time one of its values is referenced. It changes
-- nothing about what the import brings into scope; see
-- docs/code-splitting-design.md.
data Loading
  = Eager
  | Async
  deriving (Eq)


data Value = Value (A.Located N.Name) [Pattern] Expr (Maybe Signature)


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
  Constraint (A.Located Module.Prefix) (A.Located N.Name) Type


-- An overload. `abstract` declares a name in this module for other modules to
-- define; a definition writes that name qualified and gives it a body.
--
--     abstract compare : a -> a -> Order           -- in module Order
--
--     Order.compare : Card -> Card -> Order        -- in module Card
--     Order.compare a b = ...
--
data Overload
  = Abstract (A.Located N.Name) Signature
  | DefineFor (A.Located Module.Prefix) (A.Located N.Name) Signature [Pattern] Expr


data Union = Union (A.Located T.Name) [A.Located T.Var] [(A.Located N.Name, [Type])]
data TagDecl = TagDecl (A.Located N.Name) [A.Located T.Var]
data Alias = Alias (A.Located T.Name) [A.Located T.Var] Type
data Infix = Infix Op.Name Op.Associativity Op.Precedence N.Name
data Port  = Port (A.Located N.Name) Type


data Effects
  = NoEffects
  | Ports [Port]
  | Manager A.Region Manager


data Manager
  = Cmd (A.Located T.Name)
  | Sub (A.Located T.Name)
  | Fx  (A.Located T.Name) (A.Located T.Name)


data Docs
  = NoDocs A.Region [(N.Name, Comment)] [(T.Name, Comment)]
  | YesDocs Comment [(N.Name, Comment)] [(T.Name, Comment)]


newtype Comment =
  Comment P.Snippet



-- EXPOSING


data Exposing
  = Open
  | Explicit [Exposed]


data Exposed
  = Lower (A.Located N.Name)
  | Upper (A.Located T.Name) Privacy
  | Operator A.Region Op.Name


data Privacy
  = Public A.Region
  | Private
