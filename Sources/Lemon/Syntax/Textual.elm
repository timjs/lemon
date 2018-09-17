module Lemon.Syntax.Textual exposing
  ( Alternative
  , Atom(..)
  , Basic(..)
  , BasicType(..)
  , Branch
  , Declaration(..)
  , Expression(..)
  , Fields
  , Module(..)
  , Parameter
  , Pattern(..)
  , Scope
  , Statement(..)
  , Type(..)
  )

import Lemon.Names exposing (..)



-- Modules and declarations ----------------------------------------------------


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


type alias Alternative =
  ( Pattern, Expression )



-- Atoms -----------------------------------------------------------------------


type Atom
  = ABasic Basic
  | AVariable Name
  | AJust Expression
  | ANothing
  | ACons Expression Expression
  | ANil
  | ARecord (Fields Expression)


type alias Fields a =
  List ( Name, a )


type Basic
  = Bool Bool
  | Int Int
  | Float Float
  | String String



-- Statements ------------------------------------------------------------------


type Statement
  = SLet Pattern Expression
  | SBind Pattern Expression
  | SIgnore Expression
  | SPar (List (List Statement))
  | SWhen (List Branch)
  | SOn (List ( Name, Branch ))
  | SDone


type alias Branch =
  ( Expression, List Statement )



-- Patterns --------------------------------------------------------------------


type Pattern
  = PBasic Basic
  | PVariable Name
  | PJust Pattern
  | PNothing
  | PCons Pattern Pattern
  | PNil
  | PRecord (Fields Pattern)
  | PIgnore


type alias Parameter =
  ( Pattern, Type )



-- Patterns --------------------------------------------------------------------


type Type
  = TBasic BasicType
  | TVariable Name
  | TMaybe Type
  | TList Type
  | TRecord (Fields Type)
  | TTask Type
  | TArrow Type Type


type BasicType
  = TBool
  | TInt
  | TFloat
  | TString
