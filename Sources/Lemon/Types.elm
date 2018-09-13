module Lemon.Types exposing
  ( BasicType(..)
  , Type(..)
  )

import Lemon.Names exposing (Name)
import Lemon.Syntax.Common.Atom exposing (Fields)


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
