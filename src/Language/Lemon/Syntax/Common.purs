module Language.Lemon.Syntax.Common
  ( Alternative
  , Atom(..)
  , Basic(..)
  , BasicType(..)
  , Fields
  , Parameter
  , Pattern(..)
  , Statement(..)
  , Type(..)
  ) where

--XXX: Deriving Functor for Statement and Atom would be awesome here.

import Data.List (List)
import Language.Lemon.Name (Name)



-- Statements ------------------------------------------------------------------


data Statement e
  = Set Pattern e
  | Bind Pattern e
  | Do e
  | Par (List (List (Statement e)))
  | On (List { action :: Name, predicate :: e, body :: List (Statement e) })
  | When (List { predicate :: e, body :: List (Statement e) })
  | Done



-- Atoms -----------------------------------------------------------------------


data Atom e
  = Basic Basic
  | Variable Name
  | Some e
  | None
  | List (List e)
  | Record (Fields e)


type Fields a =
  List { name :: Name, value :: a }


data Basic
  = Bool Boolean
  | Int Int
  | Float Number
  | String String



-- Patterns --------------------------------------------------------------------


data Pattern
  = PBasic Basic
  | PVariable Name
  | PSome Pattern
  | PNone
  | PCons Pattern Pattern
  | PNil
  | PRecord (Fields Pattern)
  | PIgnore


type Parameter =
  { pattern :: Pattern, type :: Type }


type Alternative e =
  { pattern :: Pattern, body :: e }



-- Types -----------------------------------------------------------------------


data Type
  = TBasic BasicType
  | TVariable Name
  | TOption Type
  | TList Type
  | TRecord (Fields Type)
  | TTask Type
  | TArrow Type Type


data BasicType
  = TBool
  | TInt
  | TFloat
  | TString
