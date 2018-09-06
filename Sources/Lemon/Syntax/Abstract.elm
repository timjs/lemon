module Lemon.Syntax.Abstract exposing
  ( Declaration(..)
  , Expression(..)
  , Module(..)
  , Scope
  , empty
  )

import Lemon.Name exposing (Name)
import Lemon.Syntax.Common exposing (..)



-- Modules and Definitions -----------------------------------------------------


type Module
  = Module Scope


type alias Scope =
  List Declaration


type Declaration
  = Value Name Type Name (List Pattern) Expression



-- Expressions -----------------------------------------------------------------


type Expression
  = Atom (Atom Expression)
  | Lambda (List Parameter) Expression
  | Call Expression (List Expression)
  | Let Scope Expression
  | Case Expression (List (Alternative Expression))
  | If Expression Expression Expression
  | Sequence (List (Statement Expression))



-- Init ------------------------------------------------------------------------


empty : Scope
empty = []
