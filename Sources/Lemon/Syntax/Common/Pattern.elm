module Lemon.Syntax.Common.Pattern exposing
  ( Alternative
  , Parameter
  , Pattern(..)
  )

import Lemon.Name exposing (..)
import Lemon.Syntax.Common.Atom exposing (..)
import Lemon.Syntax.Common.Type exposing (..)


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
