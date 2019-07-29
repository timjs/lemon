module Language.Lemon.Syntax.Abstract
  ( Decl(..)
  , Expr(..)
  , Module(..)
  , Bindings
  , empty
  , module Language.Lemon.Syntax.Common
  ) where

import Preload
import Language.Lemon.Syntax.Common
import Data.List (List(..))

-- DECLARATIONS ----------------------------------------------------------------
type Bindings
  = List Decl

empty :: Bindings
empty = Nil

data Module
  = Module Bindings

data Decl
  = Value Name Type Name (List Pattern) Expr

-- EXPRESSIONS -----------------------------------------------------------------
data Expr
  = Atom (Atom Expr)
  | Lam (List Parameter) Expr
  | App Expr (List Expr)
  | Let Bindings Expr
  | Case Expr (List (Alternative Expr))
  | If Expr Expr Expr
  | Seq (List (Stmt Expr))

-- BOILERPLATE -----------------------------------------------------------------
derive instance genericModule :: Generic Module _

instance showModule :: Show Module where
  show = genericShow

derive instance genericDeclaration :: Generic Decl _

instance showDeclaration :: Show Decl where
  show x = genericShow x

derive instance genericExpression :: Generic Expr _

instance showExpression :: Show Expr where
  show x = genericShow x
