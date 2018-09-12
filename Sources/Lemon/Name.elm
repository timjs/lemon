module Lemon.Name exposing
  ( Fields
  , Name
  , isLower
  , isUpper
  )


type alias Name =
  String


type alias Fields a =
  List ( Name, a )


checkFirst : (Char -> Bool) -> String -> Bool
checkFirst pred =
  String.uncons >> Maybe.map (Tuple.first >> pred) >> Maybe.withDefault False


isUpper : Name -> Bool
isUpper = checkFirst Char.isUpper


isLower : Name -> Bool
isLower = checkFirst Char.isLower
