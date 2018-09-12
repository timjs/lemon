module Lemon.Syntax.Abstract exposing
  ( Declaration(..)
  , Expression(..)
  , Module(..)
  , Scope
  , empty
  )

--XXX: Actually we would like to re-export the Syntax.Common module...

import Lemon.Name exposing (Name)
import Lemon.Syntax.Common.Atom exposing (..)
import Lemon.Syntax.Common.Pattern exposing (..)
import Lemon.Syntax.Common.Statement exposing (..)
import Lemon.Syntax.Common.Type exposing (..)



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
