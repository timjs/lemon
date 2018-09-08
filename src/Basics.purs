module Basics
  ( module Prelude
  , module Data.Maybe
  , (>>)
  , (<<)
  ) where


import Prelude
import Data.Maybe

import Control.Semigroupoid (composeFlipped)


infixr 9 compose as <<
infixr 9 composeFlipped as >>
