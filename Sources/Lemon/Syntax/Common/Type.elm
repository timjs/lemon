module Lemon.Syntax.Common.Type exposing
  ( BasicType(..)
  , Type(..)
  )

import Lemon.Name exposing (Name)
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
