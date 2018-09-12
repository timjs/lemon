module Lemon.Syntax.Common.Statement exposing (Statement(..))

import Lemon.Name exposing (..)
import Lemon.Syntax.Common.Pattern exposing (..)



-- Statements ------------------------------------------------------------------


type Statement e
  = Set Pattern e
  | Bind Pattern e
  | Do e
  | Par (List (List (Statement e)))
  | On (List ( Name, e, List (Statement e) ))
  | When (List ( e, List (Statement e) ))
  | Done
