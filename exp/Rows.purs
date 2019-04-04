module Rows where

import Prelude

import Prim.Row (class Nub, class Union)
import Record (disjointUnion)


-- Union --

class (Union a b c, Nub c c) <= DisjointUnion a b c
instance disjointUnion  :: (Union a b c, Nub c c) => DisjointUnion a b c

infixl 5 or as .|.
or :: forall a b c. DisjointUnion a b c => Record a -> Record b -> Record c
or r1 r2 = disjointUnion r1 r2


-- Intersection --

class Intersection (a :: # Type) (b :: # Type) (c :: # Type)
-- ?how_to_make_an_instance

infixl 4 and as .&.
and :: forall a b c. Intersection a b c => Record a -> Record b -> Record c
and r1 r2 = ?how_to_do_this


-- Examples --

r1 :: {age :: Int}
r1 = {age: 30}

r2 :: {name :: String}
r2 = {name: "John"}

r3 :: {name :: String, town :: String}
r3 = {name: "Peter", town: "Amsterdam"}

r4 = r1 .|. r2 -- ==> {name: "John", age 30} :: {name :: String, age :: Int}
-- r5 = r2 .|. r3 -- ==> Type error! Both r2 and r3 contain the field (name :: String).

r6 = r1 .&. r2 -- ==> {} :: {}
r7 = r2 .&. r3 -- ==> {name: "John"} :: {name :: String}
