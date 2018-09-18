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

import Basics.Extra exposing (flip)
import Result exposing (..)
import Result.Extra exposing (..)



-- On Lists --------------------------------------------------------------------


{-| Map a function producing results on a list
and combine those into a single result (holding a list).
Also known as `traverse` on lists.

    combineMap f xs == combine (List.map f xs)

-}
combineMap : (a -> Result x b) -> List a -> Result x (List b)
combineMap f =
  combine << List.map f


{-| Pull a result out of the _first_ element of a tuple
and combine it into a result holding the tuple's values.
-}
combineFirst : ( Result x a, c ) -> Result x ( a, c )
combineFirst ( rx, y ) =
  Result.map (flip Tuple.pair y) rx



-- On Tuples -------------------------------------------------------------------


{-| Pull a result out of the _second_ element of a tuple
and combine it into a result holding the tuple's values.
Also known as `sequence` on tuples.
-}
combineSecond : ( c, Result x a ) -> Result x ( c, a )
combineSecond ( x, ry ) =
  Result.map (Tuple.pair x) ry


{-| Combine all results in a tuple
into a single result holding the tuple's values.
Also know as `bisequence` on tuples.
-}
combineBoth : ( Result x a, Result x b ) -> Result x ( a, b )
combineBoth ( rx, ry ) =
  Result.map2 Tuple.pair rx ry


{-| Map a function producing results on the _first_ element of a tuple
and then pull it out using `combineFirst`.
Also know as `sequence` on tuples.

    combineMapFirst f ( x, y )
      == combineFirst (Tuple.mapFirst f ( x, y ))
      == Result.map (flip Tuple.pair y) (f x)

-}
combineMapFirst : (a -> Result x b) -> ( a, c ) -> Result x ( b, c )
combineMapFirst f =
  combineFirst << Tuple.mapFirst f


{-| Map a function producing results on the _second_ element of a tuple
and then pull it out using `combineSecond`.
Also know as `traverse` on tuples.

    combineMapSecond f ( x, y )
      == combineSecond (Tuple.mapSecond f ( x, y ))
      == Result.map (Tuple.pair x) (f y)

-}
combineMapSecond : (a -> Result x b) -> ( c, a ) -> Result x ( c, b )
combineMapSecond f =
  combineSecond << Tuple.mapSecond f


{-| Map a function producing results on the _both_ elements of a tuple
and then pull them out using `combineBoth`.
Also know as `bitraverse` on tuples.

    combineMapBoth f g ( x, y )
      == combineBoth (Tuple.mapBoth f g ( x, y ))
      == Result.map2 Tuple.pair (f x) (g y)

-}
combineMapBoth : (a -> Result x c) -> (b -> Result x d) -> ( a, b ) -> Result x ( c, d )
combineMapBoth f g =
  combineBoth << Tuple.mapBoth f g



-- Monad -----------------------------------------------------------------------


{-| Join contained results with the same error into one result.

Usefull if you have a "result in a result":

    join <| Ok (Ok 4) == Ok 4

    join <| Ok (Err "message") == Err "message"

-}
join : Result x (Result x a) -> Result x a
join r =
  case r of
    Err x -> Err x
    Ok (Err x) -> Err x
    Ok (Ok a) -> Ok a
