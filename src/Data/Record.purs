module Data.Record
  ( module Record
  , class DisjointUnion
  , class Intersection, intersection, class RowListIntersection
  ) where


import Type.Row

import Type.Data.Boolean (class If)
import Type.Data.Ordering (class Equals, LT)
import Type.Data.Symbol (class Compare)

import Data.Function.Uncurried (Fn2, runFn2)

import Record as Record

foreign import unsafeIntersectionFn :: forall r1 r2 r3. Fn2 (Record r1) (Record r2) (Record r3)



-- DisjointUnion ---------------------------------------------------------------


class (Union a b c, Nub c c) <= DisjointUnion a b c


instance recordDisjointUnion ::
  (Union a b c, Nub c c) => DisjointUnion a b c



-- Intersection ----------------------------------------------------------------


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


class Intersection
  (l :: # Type) (r :: # Type) (i :: # Type)
  | l r -> i


instance intersectionRL ::
  ( RowToList l xs
  , RowToList r ys
  , RowListIntersection xs ys zs
  , ListToRow zs i
  ) => Intersection r l i
  -- type Intersection l r = ListToRow (RowListIntersection (RowToList r) (RowToList l))


intersection :: forall r l i. Intersection r l i => Record r -> Record l -> Record i
intersection = runFn2 unsafeIntersectionFn
