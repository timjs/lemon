module Data.Task where

import Preload

import Data.Record (class DisjointUnion, class Intersection)
import Unsafe.Coerce (unsafeCoerce)


-- Types -----------------------------------------------------------------------

type Check a = a -> Boolean
type Message = String

data Task a
  = Task a

data Store a
  = Store a

instance functorTask :: Functor Task where
  map = undefined
instance applyTask :: Apply Task where
  apply = undefined
instance applicativeTask :: Applicative Task where
  pure = undefined
instance bind :: Bind Task where
  bind = undefined


-- Editors ---------------------------------------------------------------------

enter :: forall a. Message -> {} -> Task { value :: a }
enter = undefined

edit :: forall a. Message -> { value :: a } ->Task { value :: a }
edit = undefined

view :: forall a. Message -> { value :: a } -> Task { value :: a }
view = undefined

watch :: forall a. Message -> Store a -> {} -> Task { value :: a }
watch = undefined

update :: forall a. Message -> Store a -> {} -> Task {}
update = undefined

select :: forall a. Message -> Array a -> Store (Array a) -> {}
  -> Task { value :: Array a }
select = undefined


-- Stores ----------------------------------------------------------------------

infixl 3 modify as <<-

store :: forall a. a -> Store a
store = undefined

modify :: forall a. Store a -> (a -> a) -> Task Unit
modify = undefined


-- Combinators -----------------------------------------------------------------

type Option a =
  { on :: String, when :: Boolean, cont :: Task a }

infixl 5 and as -&&-
infixl 3 or as -||-
infixl 3 pick as -??-

and :: forall a b c. DisjointUnion a b c
  => Task (Record a) -> Task (Record b) -> Task (Record c)
and = undefined

or :: forall a b c. Intersection a b c
  => Task (Record a) -> Task (Record b) -> Task (Record c)
or = undefined

pick :: forall a b c. Intersection a b c
  => Option (Record a) -> Option (Record b) -> Task (Record c)
pick = undefined

only :: forall a. Option (Record a) -> Task (Record  a)
only = undefined


-- Helpers ---------------------------------------------------------------------

always :: forall a. a -> Boolean
always = const true

undefined :: forall a. a
undefined = unsafeCoerce unit
