module Language.Lemon.Syntax.Abstract
  ( Declaration(..)
  , Expression(..)
  , Module(..)
  , Scope
  , empty
  ) where

--XXX: Actually we would like to re-export the Syntax.Common module...

import Data.List (List(..))

import Language.Lemon.Syntax.Common



-- Modules and Definitions -----------------------------------------------------


data Module
  = Module Scope


type Scope =
  List Declaration


data Declaration
  = Value Name Type Name (List Pattern) Expression



-- Expressions -----------------------------------------------------------------


data Expression
  = Atom (Atom Expression)
  | Lambda (List Parameter) Expression
  | Call Expression (List Expression)
  | Let Scope Expression
  | Case Expression (List (Alternative Expression))
  | If Expression Expression Expression
  | Sequence (List (Statement Expression))



-- Init ------------------------------------------------------------------------


empty :: Scope
empty = Nil
