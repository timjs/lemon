module Language.Lemon.Syntax.Common
  ( Alternative
  , Atom(..)
  , Basic(..)
  , BasicType(..)
  , Fields
  , Parameter
  , Pattern(..)
  , Statement(..)
  , Type(..)
  , module Language.Lemon.Name
  ) where


import Basics

import Data.List (List)
import Data.List as List

import Language.Lemon.Name



-- Statements ------------------------------------------------------------------


data Statement e
  = Set Pattern e
  | Bind Pattern e
  | Do e
  | Par (List (List (Statement e)))
  | On (List { action :: Name, predicate :: e, body :: List (Statement e) })
  | When (List { predicate :: e, body :: List (Statement e) })
  | Done

derive instance statementFunctor :: Functor Statement



-- Atoms -----------------------------------------------------------------------


type Fields a =
  List (Tuple Name a)


data Atom e
  = Basic Basic
  | Variable Name
  | None
  | Some e
  | List (List e)
  | Record (Fields e)

derive instance atomFunctor :: Functor Atom


instance atomFoldable :: Foldable Atom where

  foldl f x (Some e)    = f x e
  foldl f x (List es)   = foldl f x es
  foldl f x (Record fs) = foldl f x $ map snd fs
  foldl _ x _           = x

  foldr f = foldrDefault f

  foldMap f = foldMapDefaultL f


instance atomTraversable :: Traversable Atom where

  sequence (Basic b)    = pure $ Basic b
  sequence (Variable x) = pure $ Variable x
  sequence (None)       = pure $ None
  sequence (Some e)     = Some <$> e
  sequence (List es)    = List <$> sequence es
  sequence (Record fs)  = Record << List.zip names <$> sequence values
    where
      (Tuple names values) = List.unzip fs

  traverse = traverseDefault


data Basic
  = Bool Boolean
  | Int Int
  | Float Number
  | String String



-- Patterns --------------------------------------------------------------------


data Pattern
  = PBasic Basic
  | PVariable Name
  | PSome Pattern
  | PNone
  | PCons Pattern Pattern
  | PNil
  | PRecord (Fields Pattern)
  | PIgnore


type Parameter =
  Tuple Pattern Type


type Alternative e =
  Tuple Pattern e



-- Types -----------------------------------------------------------------------


data Type
  = TBasic BasicType
  | TVariable Name
  | TOption Type
  | TList Type
  | TRecord (Fields Type)
  | TTask Type
  | TArrow Type Type


data BasicType
  = TBool
  | TInt
  | TFloat
  | TString
