module Lemon.Types exposing
  ( BasicType(..)
  , Type(..)
  )

import Dict exposing (Dict)
import Lemon.Names exposing (Name)


type Type
  = TBasic BasicType
  | TVariable Name
  | TMaybe Type
  | TList Type
  | TRecord (Dict Name Type)
  | TTask Type
  | TArrow Type Type


type BasicType
  = TBool
  | TInt
  | TFloat
  | TString
