module Preload
  ( module Reexport
  , neutral
  , (<<), (>>)
  , (**), type (**), type (++)
  , (<&>), pair, none
  ) where


import Prelude hiding (mempty, (<<<), (>>>)) as Reexport

import Control.Alternative as Reexport

import Data.Either hiding (Either) as Reexport
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
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))


-- Operators for composition --

infixr 9 Reexport.compose as <<
infixr 9 composeFlipped as >>


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
