module RowLists where

import Prelude

import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.Symbol as Symbol
import Record as Record

import Type.Data.Boolean (class If)
import Type.Data.Ordering (class Equals)
import Type.Prelude (class IsSymbol, class RowToList, LT, Proxy(..), RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil, kind RowList)

import Unsafe.Coerce (unsafeCoerce)


class RowListIntersection (xs :: RowList) (ys :: RowList) (res :: RowList)
  | xs ys -> res

instance rliNilXs ::
  RowListIntersection Nil (Cons name ty ys) Nil
else instance rliNilYs ::
  RowListIntersection (Cons name ty xs) Nil Nil
else instance rliNilNil ::
  RowListIntersection Nil Nil Nil
else instance rliMatch ::
  ( RowListIntersection xs ys zs
  ) => RowListIntersection (Cons name ty xs) (Cons name ty ys) (Cons name ty zs)
else instance rliConsCons ::
  ( Symbol.Compare xname yname ord
  , Equals ord LT isLt
  , If isLt
      (RLProxy xs)
      (RLProxy (Cons xname xty xs))
      (RLProxy xs')
  , If isLt
      (RLProxy (Cons yname yty ys))
      (RLProxy ys)
      (RLProxy ys')
  , RowListIntersection xs' ys' res
  ) => RowListIntersection (Cons xname xty xs) (Cons yname yty ys) res

rowListIntersection :: forall x xs y ys zs.
  RowToList x xs => RowToList y ys => RowListIntersection xs ys zs =>
  { | x } -> { | y } -> RLProxy zs
rowListIntersection _ _ = RLProxy


-- Tests -----------------------------------------------------------------------

expectInferred :: forall a. Proxy a -> a -> Unit
expectInferred = unsafeCoerce unit

testA :: Unit
testA =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))
    actual = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2 }
  in
    expectInferred expected actual

testB :: Unit
testB =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))
    actual = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2, c: "c" }
  in
    expectInferred expected actual

testC :: Unit
testC =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))
    actual = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2 }
  in
    expectInferred expected actual

testD :: Unit
testD =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))
    actual = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2, d: "d" }
  in
    expectInferred expected actual
