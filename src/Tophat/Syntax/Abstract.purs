module Tophat.Syntax.Abstract
  ( Decl(..)
  , Expr(..)
  , Module(..)
  , Bindings
  , empty
  , module Tophat.Syntax.Common
  ) where

import Preload
import Tophat.Syntax.Common (Alternative, Atom(..), Dict, Guarded, Mode(..), Name, Parameter, Pattern(..), Prim(..), PrimType(..), Stmt(..), Type(..), isLower, isUpper)
import Data.List (List(..))

-- DECLARATIONS ----------------------------------------------------------------
data Module
  = Module Bindings

type Bindings
  = List Decl

empty :: Bindings
empty = Nil

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
