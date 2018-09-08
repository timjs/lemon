module Language.Lemon.Name
  ( Name
  , isLower
  , isUpper
  ) where


import Basics

import Data.String.CodeUnits as String
import Data.Char.Unicode as Char


type Name =
  String


checkFirst :: (Char -> Boolean) -> Name -> Boolean
checkFirst pred =
  String.uncons >> map (_.head >> pred) >> fromMaybe false


isUpper :: Name -> Boolean
isUpper = checkFirst Char.isUpper


isLower :: Name -> Boolean
isLower = checkFirst Char.isLower
