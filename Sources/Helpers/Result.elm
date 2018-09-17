module Helpers.Result exposing
  ( combineBoth
  , combineMap
  , combineMapBoth
  , combineMapSecond
  , combineSecond
  , join
  )

import Result exposing (..)
import Result.Extra exposing (..)



-- Traversable on  Lists -------------------------------------------------------


{-| Actually `traverse` on Lists and Results
-}
combineMap : (a -> Result x b) -> List a -> Result x (List b)
combineMap g xs =
  combine (List.map g xs)



-- Traversable on Tuples -------------------------------------------------------


{-| Actually `sequence` on Tuples and Results
-}
combineSecond : ( c, Result x a ) -> Result x ( c, a )
combineSecond ( x, ry ) =
  map (Tuple.pair x) ry


combineBoth : ( Result x a, Result x b ) -> Result x ( a, b )
combineBoth ( rx, ry ) =
  map2 Tuple.pair rx ry


{-| Actually `traverse` on Tuples and Results
-}
combineMapSecond : (a -> Result x b) -> ( c, a ) -> Result x ( c, b )
combineMapSecond f ( x, y ) =
  map (Tuple.pair x) (f y)


{-| Actually `bitraverse` on Tuples and Results
-}
combineMapBoth : (a -> Result x c) -> (b -> Result x d) -> ( a, b ) -> Result x ( c, d )
combineMapBoth f g ( x, y ) =
  map2 Tuple.pair (f x) (g y)



-- Monad -----------------------------------------------------------------------


join : Result x (Result x a) -> Result x a
join r =
  case r of
    Err x ->
      Err x
    Ok (Err x) ->
      Err x
    Ok (Ok a) ->
      Ok a
