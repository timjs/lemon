module RowLists where

import Type.Prelude

import Type.Data.Boolean (class If)
import Type.Data.Ordering (class Equals)
import Type.Row (Cons, Nil, kind RowList)

import Data.Function.Uncurried (Fn2, runFn2)

foreign import unsafeIntersectionFn :: forall r1 r2 r3. Fn2 (Record r1) (Record r2) (Record r3)


class RowListIntersection (xs :: RowList) (ys :: RowList) (res :: RowList)
  | xs ys -> res

instance rliNilXs ::
  RowListIntersection Nil (Cons name ty ys) Nil
  -- type RowListIntersection Nil (Cons name ty ys) = Nil
else instance rliNilYs ::
  RowListIntersection (Cons name ty xs) Nil Nil
  -- type RowListIntersection (Cons name ty xs) Nil = Nil
else instance rliNilNil ::
  RowListIntersection Nil Nil Nil
  -- type RowListIntersection Nil Nil = Nil
else instance rliMatch ::
  ( RowListIntersection xs ys zs
  ) => RowListIntersection (Cons name ty xs) (Cons name ty ys) (Cons name ty zs)
  -- type RowListIntersection (Cons name ty xs) (Cons name ty ys) = (Cons name ty (RowListIntersection xs ys))
else instance rliConsCons ::
  ( Compare xname yname ord
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
  -- type RowListIntersection (Cons xname xty xs) (Cons yname yty ys) =
  --   type IsLt = Equals (Symbol.Compare xname yname) LT
  --   in RowListIntersection (If IsLt xs (Cons xname xty xs)) (If IsLt (Cons yname yty ys) ys)

-- rowListIntersection :: forall x xs y ys zs
--   .  RowToList x xs => RowToList y ys => RowListIntersection xs ys zs
--   => { | x } -> { | y } -> RLProxy zs
-- rowListIntersection _ _ = RLProxy

class Intersection
  (l :: # Type) (r :: # Type) (u :: # Type)
  | l r -> u
  where
  intersection :: Record r -> Record l -> Record u

instance intersectionRL ::
  ( RowToList l xs
  , RowToList r ys
  , RowListIntersection xs ys zs
  , ListToRow zs u
  ) => Intersection r l u
  where
    -- type Intersection l r = ListToRow (RowListIntersection (RowToList r) (RowToList l))
    intersection = runFn2 unsafeIntersectionFn


-- Tests -----------------------------------------------------------------------


r1 :: {age :: Int}
r1 = {age: 30}

r2 :: {name :: String}
r2 = {name: "John"}

r3 :: {name :: String, town :: String}
r3 = {name: "Peter", town: "Amsterdam"}

-- r4 = r1 .|. r2 -- ==> {name: "John", age 30} :: {name :: String, age :: Int}
-- -- r5 = r2 .|. r3 -- ==> Type error! Both r2 and r3 contain the field (name :: String).

r6 = intersection r1 r2 -- ==> {} :: {}
r7 = intersection r2 r3 -- ==> {name: "John"} :: {name :: String}

{-

expectInferred :: forall a. Proxy a -> a -> Unit
expectInferred = unsafeCoerce 0

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

-}
