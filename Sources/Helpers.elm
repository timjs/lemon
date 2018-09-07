module Helpers exposing (combine)

-- Results ---------------------------------------------------------------------


combine : List (Result x a) -> Result x (List a)
combine = List.foldr (Result.map2 (::)) (Ok [])
