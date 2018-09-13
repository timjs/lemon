module Lemon.Syntax.Common.Pattern exposing
  ( Alternative
  , Parameter
  , Pattern(..)
  )

import Lemon.Names exposing (Name)
import Lemon.Syntax.Common.Atom exposing (Basic, Fields)
import Lemon.Types exposing (Type)



--XXX: We'd like to factor out the fields of a record,
--     as they are Lists in the Source syntax
--     and Dicts in the Canonical one.
--     However, Elm doesn't support higher kinded types :-(


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


type alias Alternative e =
  ( Pattern, e )
