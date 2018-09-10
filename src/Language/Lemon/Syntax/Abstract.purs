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



-- DECLARATIONS ----------------------------------------------------------------


type Scope = List Declaration


empty :: Scope
empty = Nil


data Module
  = Module Scope


data Declaration
  = Value Name Type Name (List Pattern) Expression



-- EXPRESSIONS -----------------------------------------------------------------


data Expression
  = Atom (Atom Expression)
  | Lambda (List Parameter) Expression
  | Call Expression (List Expression)
  | Let Scope Expression
  | Case Expression (List (Alternative Expression))
  | If Expression Expression Expression
  | Sequence (List (Statement Expression))
