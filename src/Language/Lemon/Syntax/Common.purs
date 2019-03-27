module Language.Lemon.Syntax.Common
  ( Alternative
  , Atom(..)
  , Prim(..)
  , PrimType(..)
  , Fields
  , Parameter
  , Pattern(..)
  , Stmt(..)
  , Type(..)
  , module Language.Lemon.Names
  ) where


import Basics

import Data.List (List)
import Data.List as List

import Language.Lemon.Names (Name, isLower, isUpper)



-- STATEMENTS ------------------------------------------------------------------


data Stmt e
  = Set Pattern e
  | Bind Pattern e
  | Par (List (List (Stmt e)))
  | On (List { action :: Name, predicate :: e, body :: List (Stmt e) })
  | When (List { predicate :: e, body :: List (Stmt e) })
  | Done

derive instance genericCmd :: Generic (Stmt e) _
derive instance functorCmd :: Functor Stmt
instance showCmd :: Show e => Show (Stmt e) where show = genericShow


instance foldableCmd :: Foldable Stmt where
  foldMap f (Set _ e)  = f e
  foldMap f (Bind _ e) = f e
  foldMap f (Par bs)   = foldMap (foldMap (foldMap f)) bs
  foldMap f (On bs)    = foldMap (foldMapGuarded f) bs
  foldMap f (When bs)  = foldMap (foldMapGuarded f) bs
  foldMap f (Done)     = neutral

  foldl f = foldlDefault f

  foldr f = foldrDefault f


instance traversableCmd :: Traversable Stmt where
  sequence (Set p e)  = Set p <$> e
  sequence (Bind p e) = Bind p <$> e
  sequence (Par bs)   = Par <$> sequence (map sequence (map (map sequence) bs))
  sequence (On bs)    = On <$> sequence (map sequenceOn bs)
  sequence (When bs)  = When <$> sequence (map sequenceWhen bs)
  sequence (Done)     = pure $ Done

  traverse = traverseDefault


type Guarded r e = { predicate :: e, body :: List (Stmt e) | r }


foldMapGuarded :: forall e r m. Monoid m => (e -> m) -> Guarded r e -> m
foldMapGuarded f { predicate: e, body: es } = f e <> foldMap (foldMap f) es


sequenceOn :: forall e f. Applicative f => Guarded (action :: Name) (f e) -> f (Guarded (action :: Name) e)
sequenceOn { action, predicate: e, body: es } = ado
  { predicate, body } <- sequenceWhen { predicate: e, body: es }
  in { action, predicate, body }


sequenceWhen :: forall e f. Applicative f => Guarded () (f e) -> f (Guarded () e)
sequenceWhen { predicate: e, body: es } =
  { predicate: _, body: _ } <$> e <*> sequence (map sequence es)
  -- predicate <- e
  -- body <- es'
  -- in { predicate, body }
  -- where
  --   es' = sequence (map sequence es)
  --


-- ATOMS -----------------------------------------------------------------------


type Fields a = List { name :: Name, value :: a }


data Atom e
  = Prim Prim
  | Var Name
  | None
  | Some e
  | List (List e)
  | Record (Fields e)

derive instance genericAtom :: Generic (Atom e) _
derive instance functorAtom :: Functor Atom
instance showAtom :: Show e => Show (Atom e) where show = genericShow


instance foldableAtom :: Foldable Atom where
  foldMap f (Some e)    = f e
  foldMap f (List es)   = foldMap f es
  foldMap f (Record fs) = foldMap f $ map _.value fs
  foldMap f _           = neutral

  foldl f = foldlDefault f

  foldr f = foldrDefault f


instance traversableAtom :: Traversable Atom where
  sequence (Prim b)    = pure $ Prim b
  sequence (Var x)     = pure $ Var x
  sequence (None)      = pure $ None
  sequence (Some e)    = Some <$> e
  sequence (List es)   = List <$> sequence es
  sequence (Record fs) = Record << List.zipWith { name: _, value: _ } names <$> sequence values
    where
      names = map _.name fs
      values = map _.value fs

  traverse = traverseDefault


data Prim
  = B Boolean
  | I Int
  | F Number
  | S String

derive instance genericPrim :: Generic Prim _
instance showPrim :: Show Prim where show = genericShow



-- PATTERNS --------------------------------------------------------------------


data Pattern
  = PPrim Prim
  | PVar Name
  | PSome Pattern
  | PNone
  | PCons Pattern Pattern
  | PNil
  | PRecord (Fields Pattern)
  | PIgnore

derive instance genericPattern :: Generic Pattern _
instance showPattern :: Show Pattern where show x = genericShow x


--NOTE: Leave these types as tuples so that we can use the standard Foldable and Traversable instances
type Parameter = Pattern ** Type
type Alternative e = Pattern ** e



-- TYPES -----------------------------------------------------------------------


data Type
  = TPrim PrimType
  | TVar Name
  | TOption Type
  | TList Type
  | TRecord (Fields Type)
  | TTask Type
  | TArrow Type Type

derive instance typeGeneric :: Generic Type _
instance showType :: Show Type where show x = genericShow x


data PrimType
  = TBool
  | TInt
  | TFloat
  | TString

derive instance genericPrimType :: Generic PrimType _
instance showPrimType :: Show PrimType where show = genericShow
