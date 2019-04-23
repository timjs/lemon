module Preface
  ( module Reexport
  , neutral
  , (<<), (>>)
  , (..)
  , undefined
  ) where

import Prelude hiding (mempty, (<<<), (>>>), when) as Reexport

import Data.Array hiding ((..)) as Reexport
import Data.Enum (class Enum) as Reexport
import Data.Maybe as Reexport
import Data.Tuple hiding (Tuple(Tuple)) as Reexport
import Data.Task as Reexport


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

-- Operator and functions for monoidal functors --

infixl 5 pair as <&>

pair :: forall f a b. Reexport.Apply f => f a -> f b -> f (a ** b)
pair x y = (**) Reexport.<$> x Reexport.<*> y

none :: forall f. Reexport.Applicative f => f Reexport.Unit
none = Reexport.pure Reexport.unit


-- Undefined --

undefined :: forall a. Warn (Text "Undefined function in code") => a
undefined = unsafeCoerce Reexport.unit
