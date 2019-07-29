module Preload
  ( module Reexport
  , neutral
  , (<<)
  , (>>)
  , (..)
  , (**)
  , type (**)
  , type (++)
  , Nat
  , nat
  , unnat
  , (<&>)
  , pair
  , none
  , undefined
  ) where

import Prelude hiding (mempty, (<<<), (>>>)) as Reexport
import Control.Alternative as Reexport
import Data.Either hiding (Either) as Reexport
import Data.Enum (class Enum) as Reexport
import Data.Functor as Reexport
import Data.Foldable as Reexport
import Data.Maybe as Reexport
import Data.Traversable as Reexport
import Data.Tuple hiding (Tuple(Tuple)) as Reexport
import Data.Generic.Rep (class Generic) as Reexport
import Data.Generic.Rep.Show (genericShow) as Reexport
-- RENAMES ---------------------------------------------------------------------
import Control.Semigroupoid (composeFlipped)
import Data.Either (Either)
import Data.Enum (enumFromTo)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))
import Unsafe.Coerce (unsafeCoerce)
import Prim.TypeError (class Warn, Text)

-- Operators for composition --
infixr 9 Reexport.compose as <<

infixr 9 composeFlipped as >>

-- Operators for enums --
infix 8 enumFromTo as ..

-- Operators for tuples and eithers --
infixr 6 Tuple as **

infixr 6 type Tuple as **

infixr 5 type Either as ++

-- Method renames for monoids --
neutral :: forall m. Reexport.Monoid m => m
neutral = mempty

-- ADDITIONS -------------------------------------------------------------------
-- Natural numbers --
newtype Nat
  = Nat Int

nat :: Int -> Nat
nat i
  | i Reexport.>= 0 = Nat i
  | Reexport.otherwise = Nat 0

unnat :: Nat -> Int
unnat (Nat i) = i

instance showNat :: Reexport.Show Nat where
  show (Nat i) = "+" Reexport.<> Reexport.show i

instance eqNat :: Reexport.Eq Nat where
  eq (Nat i) (Nat j) = i Reexport.== j

instance ordNat :: Reexport.Ord Nat where
  compare (Nat i) (Nat j) = Reexport.compare i j

-- Operator and functions for monoidal functors --
infixl 5 pair as <&>

pair :: forall f a b. Reexport.Apply f => f a -> f b -> f (a ** b)
pair x y = (**) Reexport.<$> x Reexport.<*> y

none :: forall f. Reexport.Applicative f => f Reexport.Unit
none = Reexport.pure Reexport.unit

-- Undefined --
undefined :: forall a. Warn (Text "Undefined function in code") => a
undefined = unsafeCoerce Reexport.unit
