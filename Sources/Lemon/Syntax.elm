module Lemon.Syntax exposing
  ( Alternative
  , Atom(..)
  , Basic(..)
  , BasicType(..)
  , Declaration(..)
  , Expression(..)
  , Fields
  , Module(..)
  , Parameter
  , Pattern(..)
  , Scope
  , Statement(..)
  , Type(..)
  , empty
  )

import Lemon.Name exposing (..)



-- Modules and Definitions -----------------------------------------------------


type Module
  = Module Scope


type alias Scope =
  List Declaration


type Declaration
  = Value Name Type Name (List Pattern) Expression



-- Expressions -----------------------------------------------------------------


type Expression
  = Atom Atom
  | Lambda (List Parameter) Expression
  | Call Expression (List Expression)
  | Let Scope Expression
  | Case Expression (List Alternative)
  | If Expression Expression Expression
  | Sequence (List Statement)


type alias Parameter =
  ( Pattern, Type )


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
  | Record (Fields Expression)


type alias Fields a =
  List ( Name, a )


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
  | PRecord (Fields Pattern)
  | PIgnore



-- Types -----------------------------------------------------------------------


type Type
  = TBasic BasicType
  | TVariable Name
  | TOption Type
  | TList Type
  | TRecord (Fields Type)
  | TTask Type
  | TArrow Type Type


type BasicType
  = TBool
  | TInt
  | TFloat
  | TString



-- Init ------------------------------------------------------------------------


empty : Scope
empty = []
