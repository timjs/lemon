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

import Language.Lemon.Name (Name, isLower, isUpper)



-- STATEMENTS ------------------------------------------------------------------


data Statement e
  = Set Pattern e
  | Bind Pattern e
  | Do e
  | Par (List (List (Statement e)))
  | On (List { action :: Name, predicate :: e, body :: List (Statement e) })
  | When (List { predicate :: e, body :: List (Statement e) })
  | Done

derive instance statementFunctor :: Functor Statement


instance statementFoldable :: Foldable Statement where
  foldMap f (Set _ e)  = f e
  foldMap f (Bind _ e) = f e
  foldMap f (Do e)     = f e
  foldMap f (Par bs)   = foldMap (foldMap (foldMap f)) bs
  foldMap f (On bs)    = foldMap (foldMapGuarded f) bs
  foldMap f (When bs)  = foldMap (foldMapGuarded f) bs
  foldMap f (Done)     = neutral

  foldl f = foldlDefault f

  foldr f = foldrDefault f


instance statementTraversable :: Traversable Statement where
  sequence (Set p e)  = Set p <$> e
  sequence (Bind p e) = Bind p <$> e
  sequence (Do e)     = Do <$> e
  sequence (Par bs)   = Par <$> sequence (map sequence (map (map sequence) bs))
  sequence (On bs)    = On <$> sequence (map sequenceOn bs)
  sequence (When bs)  = When <$> sequence (map sequenceWhen bs)
  sequence (Done)     = pure $ Done

  traverse = traverseDefault


type Guarded r e = { predicate :: e, body :: List (Statement e) | r }


foldMapGuarded :: forall e r m. Monoid m =>
  (e -> m) -> Guarded r e -> m
foldMapGuarded f { predicate: e, body: es } = f e <> foldMap (foldMap f) es


sequenceOn :: forall e f. Applicative f =>
  Guarded (action :: Name) (f e) -> f (Guarded (action :: Name) e)
sequenceOn { action, predicate: e, body: es } = ado
  { predicate, body } <- sequenceWhen { predicate: e, body: es }
  in { action, predicate, body }


sequenceWhen :: forall e f. Applicative f =>
  Guarded () (f e) -> f (Guarded () e)
sequenceWhen { predicate: e, body: es } = ado
  predicate <- e
  body <- es'
  in { predicate, body }
  where
    es' = sequence (map sequence es)



-- ATOMS -----------------------------------------------------------------------


--FIXME: Use record again
type Fields a = List (Tuple Name a)


data Atom e
  = Basic Basic
  | Variable Name
  | None
  | Some e
  | List (List e)
  | Record (Fields e)

derive instance atomFunctor :: Functor Atom


instance atomFoldable :: Foldable Atom where
  foldMap f (Some e)    = f e
  foldMap f (List es)   = foldMap f es
  foldMap f (Record fs) = foldMap f $ map snd fs
  foldMap f _           = neutral

  foldl f = foldlDefault f

  foldr f = foldrDefault f


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



-- PATTERNS --------------------------------------------------------------------


data Pattern
  = PBasic Basic
  | PVariable Name
  | PSome Pattern
  | PNone
  | PCons Pattern Pattern
  | PNil
  | PRecord (Fields Pattern)
  | PIgnore


type Parameter = Tuple Pattern Type


type Alternative e = Tuple Pattern e



-- TYPES -----------------------------------------------------------------------


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
