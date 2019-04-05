module Language.Lemon.Syntax.Common
  ( Alternative
  , Atom(..)
  , Mode(..)
  , Prim(..)
  , PrimType(..)
  , Fields
  , Parameter
  , Pattern(..)
  , Stmt(..)
  , Type(..)
  , module Language.Lemon.Names
  ) where


import Preload

import Data.List (List)
import Language.Lemon.Names (Name, isLower, isUpper)

import Data.List as List



-- STATEMENTS ------------------------------------------------------------------


data Stmt e
  = Set Pattern e
  | Bind Pattern e
  | Par Mode (List (List (Stmt e)))
  | On (List { action :: Name, guard :: e, body :: List (Stmt e) })
  | When (List { guard :: e, body :: List (Stmt e) })
  | Done


data Mode
  = All
  | Any


type Guarded r e = { guard :: e, body :: List (Stmt e) | r }



-- ATOMS -----------------------------------------------------------------------


type Fields e = List { name :: Name, value :: e }


data Atom e
  = Prim Prim
  | Var Name
  | None
  | Some e
  | List (List e)
  | Record (Fields e)


data Prim
  = B Boolean
  | I Int
  | F Number
  | S String



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


data PrimType
  = TBool
  | TInt
  | TFloat
  | TString



-- FOLDABLE & TRAVERSABLE ------------------------------------------------------


instance foldableStmt :: Foldable Stmt where
  foldMap f (Set _ e)  = f e
  foldMap f (Bind _ e) = f e
  foldMap f (Par _ bs) = foldMap (foldMap (foldMap f)) bs
  foldMap f (On bs)    = foldMap (foldMapGuarded f) bs
  foldMap f (When bs)  = foldMap (foldMapGuarded f) bs
  foldMap f (Done)     = neutral

  foldl f = foldlDefault f

  foldr f = foldrDefault f


instance traversableStmt :: Traversable Stmt where
  sequence (Set p e)  = Set p <$> e
  sequence (Bind p e) = Bind p <$> e
  sequence (Par m bs) = Par m <$> sequence (map sequence (map (map sequence) bs))
  sequence (On bs)    = On <$> sequence (map sequenceOn bs)
  sequence (When bs)  = When <$> sequence (map sequenceWhen bs)
  sequence (Done)     = pure $ Done

  traverse = traverseDefault


foldMapGuarded :: forall e r m. Monoid m => (e -> m) -> Guarded r e -> m
foldMapGuarded f { guard: e, body: es } = f e <> foldMap (foldMap f) es


sequenceOn :: forall e f. Applicative f => Guarded (action :: Name) (f e) -> f (Guarded (action :: Name) e)
sequenceOn { action, guard: e, body: es } = ado
  { guard, body } <- sequenceWhen { guard: e, body: es }
  in { action, guard, body }


sequenceWhen :: forall e f. Applicative f => Guarded () (f e) -> f (Guarded () e)
sequenceWhen { guard: e, body: es } =
  { guard: _, body: _ } <$> e <*> sequence (map sequence es)
  -- guard <- e
  -- body <- es'
  -- in { guard, body }
  -- where
  --   es' = sequence (map sequence es)
  --


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



-- BOILERPLATE -----------------------------------------------------------------


derive instance genericStmt :: Generic (Stmt e) _
derive instance functorStmt :: Functor Stmt
instance showStmt :: Show e => Show (Stmt e) where show = genericShow

derive instance genericMode :: Generic Mode _
instance showMode :: Show Mode where show = genericShow

derive instance genericAtom :: Generic (Atom e) _
derive instance functorAtom :: Functor Atom
instance showAtom :: Show e => Show (Atom e) where show = genericShow

derive instance genericPrim :: Generic Prim _
instance showPrim :: Show Prim where show = genericShow

derive instance genericPattern :: Generic Pattern _
instance showPattern :: Show Pattern where show x = genericShow x

derive instance typeGeneric :: Generic Type _
instance showType :: Show Type where show x = genericShow x

derive instance genericPrimType :: Generic PrimType _
instance showPrimType :: Show PrimType where show = genericShow
