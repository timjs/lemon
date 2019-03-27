module Language.Lemon.Syntax.Abstract
  ( Declaration(..)
  , Expression(..)
  , Module(..)
  , Scope
  , empty
  ) where


--XXX: Actually we would like to re-export the Syntax.Common module...

import Basics

import Data.List (List(..))
import Language.Lemon.Syntax.Common (Alternative, Atom, Name, Parameter, Pattern, Statement, Type)



-- DECLARATIONS ----------------------------------------------------------------


type Scope = List Declaration


empty :: Scope
empty = Nil


data Module
  = Module Scope

derive instance genericModule :: Generic Module _
instance showModule :: Show Module where show = genericShow


data Declaration
  = Value Name Type Name (List Pattern) Expression

derive instance genericDeclaration :: Generic Declaration _
instance showDeclaration :: Show Declaration where show x = genericShow x



-- EXPRESSIONS -----------------------------------------------------------------


data Expression
  = Atom (Atom Expression)
  | Lambda (List Parameter) Expression
  | Call Expression (List Expression)
  | Let Scope Expression
  | Case Expression (List (Alternative Expression))
  | If Expression Expression Expression
  | Sequence (List (Statement Expression))

derive instance genericExpression :: Generic Expression _
instance showExpression :: Show Expression where show x = genericShow x
