module Helpers.Function exposing
  ( curry
  , flip
  , uncurry
  )


flip : (a -> b -> c) -> (b -> a -> c)
flip f x y =
  f y x


curry : (( a, b ) -> c) -> (a -> b -> c)
curry f x y =
  f ( x, y )


uncurry : (a -> b -> c) -> (( a, b ) -> c)
uncurry f ( x, y ) =
  f x y
