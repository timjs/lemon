module Lemon.Syntax exposing
  ( Alternative
  , Atom(..)
  , Basic(..)
  , BasicType(..)
  , Declaration(..)
  , Expression(..)
  , Module(..)
  , Pattern(..)
  , Scope
  , Statement(..)
  , Type(..)
  )

import Dict exposing (Dict)
import Lemon.Name exposing (..)



-- Modules and Definitions -----------------------------------------------------


type Module
  = Module Scope


type alias Scope =
  Dict Name Declaration


type Declaration
  = Value Type Expression



-- Expressions -----------------------------------------------------------------


type Expression
  = Atom Atom
  | Lambda Pattern Type Expression
  | Call Expression Expression
  | Let Scope Expression
  | Case Expression (List Alternative)
  | If Expression Expression Expression
  | Sequence (List Statement)


type alias Alternative =
  ( Pattern, Expression )


type Statement
  = Set Pattern Expression
  | Bind Pattern Expression
  | Do Expression
  | Par (List (List Statement))
  | On (List ( Name, Expression, List Statement ))
  | When (List ( Expression, List Statement ))
  | Done



-- Atoms -----------------------------------------------------------------------


type Atom
  = Basic Basic
  | Variable Name
  | Some Expression
  | None
  | List (List Expression)
  | Record (Dict Name Expression)


type Basic
  = Bool Bool
  | Int Int
  | Float Float
  | String String



-- Patterns --------------------------------------------------------------------


type Pattern
  = PBasic Basic
  | PVariable Name
  | PSome Pattern
  | PNone
  | PCons Pattern Pattern
  | PNil
  | PRecord (Dict Name Pattern)
  | PIgnore



-- Types -----------------------------------------------------------------------


type Type
  = TBasic BasicType
  | TVariable Name
  | TOption Type
  | TList Type
  | TRecord (Dict Name Type)
  | TTask Type
  | TArrow Type Type


type BasicType
  = TBool
  | TInt
  | TFloat
  | TString
