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
  | PJust Pattern
  | PNothing
  | PCons Pattern Pattern
  | PEnd
  | PRecord (Fields Pattern)
  | PIgnore


type alias Parameter =
  ( Pattern, Type )


type alias Alternative e =
  ( Pattern, e )
