module Helpers exposing (combine)

-- Tuples ----------------------------------------------------------------------


combine : ( a, Result x b ) -> Result x ( a, b )
combine ( a, rb ) =
  case rb of
    Ok b ->
      Ok ( a, b )
    Err x ->
      Err x
