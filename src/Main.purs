module Main where

import Preload
import Data.Record (disjointUnion, intersection)

-- Tests -----------------------------------------------------------------------
r1 :: { age :: Int }
r1 = { age: 30 }

r2 :: { name :: String }
r2 = { name: "John" }

r3 :: { name :: String, town :: String }
r3 = { name: "Peter", town: "Amsterdam" }

r4 = disjointUnion r1 r2 -- ==> {name: "John", age 30} :: {name :: String, age :: Int}

-- r5 = disjointUnion r2 r3 -- ==> Type error! Both r2 and r3 contain the field (name :: String).
r6 = intersection r1 r2 -- ==> {} :: {}

r7 = intersection r2 r3 -- ==> {name: "John"} :: {name :: String}

r10 :: { name :: Boolean }
r10 = { name: true }

r11 = intersection r2 r10
 -- r12 = disjointUnion r2 r10 --