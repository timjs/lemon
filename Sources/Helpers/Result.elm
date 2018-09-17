module Helpers.Result exposing (join)


join : Result x (Result x a) -> Result x a
join r =
  case r of
    Err x ->
      Err x
    Ok (Err x) ->
      Err x
    Ok (Ok a) ->
      Ok a
