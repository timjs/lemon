module Lemon.Syntax.Common exposing
  ( Alternative
  , BasicType(..)
  , Parameter
  , Pattern(..)
  , Statement(..)
  , Type(..)
  )

--XXX: Deriving Functor for Statement and Atom would be awesome here.

import Lemon.Name exposing (Name)
import Lemon.Syntax.Common.Atom as Atom exposing (..)



-- Statements ------------------------------------------------------------------


type Statement e
  = Set Pattern e
  | Bind Pattern e
  | Do e
  | Par (List (List (Statement e)))
  | On (List ( Name, e, List (Statement e) ))
  | When (List ( e, List (Statement e) ))
  | Done



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


type alias Parameter =
  ( Pattern, Type )


type alias Alternative e =
  ( Pattern, e )



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
