module Basics
  ( module Prelude
  , module Reexport
  , neutral
  , (<<), (>>)
  , (:)
  ) where


import Prelude

import Control.Alternative as Reexport

import Data.Either as Reexport
import Data.Functor as Reexport
import Data.Foldable as Reexport
import Data.Maybe as Reexport
import Data.Traversable as Reexport
import Data.Tuple as Reexport

import Control.Semigroupoid (composeFlipped)



-- EXTRAS ----------------------------------------------------------------------

infixr 9 compose as <<
infixr 9 composeFlipped as >>

infixr 6 Reexport.Tuple as :

neutral :: forall m. Monoid m => m
neutral = mempty
