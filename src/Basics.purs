module Basics
  ( module Prelude
  , module Reexport
  , (<<), (>>)
  -- , (<|), (|>)
  ) where


import Prelude

import Control.Alternative as Reexport

import Data.Maybe as Reexport
import Data.Either as Reexport
import Data.Foldable as Reexport
import Data.Traversable as Reexport


import Control.Semigroupoid (composeFlipped)
-- import Data.Function (applyFlipped)


infixr 9 compose as <<
infixr 9 composeFlipped as >>

-- infixr 0 apply as <|
-- infixl 1 applyFlipped as |>
