module Helpers.Result exposing
  ( combineBoth
  , combineFirst
  , combineMap
  , combineMapBoth
  , combineMapFirst
  , combineMapSecond
  , combineSecond
  , join
  )

import Result exposing (..)
import Result.Extra exposing (..)


swap ( x, y ) =
  ( y, x )



-- Traversable on  Lists -------------------------------------------------------


{-| Actually `traverse` on Lists and Results
-}
combineMap : (a -> Result x b) -> List a -> Result x (List b)
combineMap g xs =
  combine (List.map g xs)



-- Traversable on Tuples -------------------------------------------------------


{-| Actually `sequence` on Tuples and Results
-}
combineFirst : ( Result x a, c ) -> Result x ( a, c )
combineFirst = map swap << combineSecond << swap


{-| Actually `sequence` on Tuples and Results
-}
combineSecond : ( c, Result x a ) -> Result x ( c, a )
combineSecond ( x, ry ) =
  map (Tuple.pair x) ry


combineBoth : ( Result x a, Result x b ) -> Result x ( a, b )
combineBoth ( rx, ry ) =
  map2 Tuple.pair rx ry


combineMapFirst : (a -> Result x b) -> ( a, c ) -> Result x ( b, c )
combineMapFirst f xy =
  combineFirst (Tuple.mapFirst f xy)


{-| Actually `traverse` on Tuples and Results

    combineMapSecond f ( x, y ) =
      map (Tuple.pair x) (f y)

-}
combineMapSecond : (a -> Result x b) -> ( c, a ) -> Result x ( c, b )
combineMapSecond f xy =
  combineSecond (Tuple.mapSecond f xy)


{-| Actually `bitraverse` on Tuples and Results

    combineMapBoth f g ( x, y ) =
      map2 Tuple.pair (f x) (g y)

-}
combineMapBoth : (a -> Result x c) -> (b -> Result x d) -> ( a, b ) -> Result x ( c, d )
combineMapBoth f g xy =
  combineBoth (Tuple.mapBoth f g xy)



-- Monad -----------------------------------------------------------------------


join : Result x (Result x a) -> Result x a
join r =
  case r of
    Err x -> Err x
    Ok (Err x) -> Err x
    Ok (Ok a) -> Ok a
