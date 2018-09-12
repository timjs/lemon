module Helpers.Tuple exposing (combineSecond)


combineSecond : ( a, Result x b ) -> Result x ( a, b )
combineSecond ( a, rb ) =
  case rb of
    Ok b ->
      Ok ( a, b )
    Err x ->
      Err x
