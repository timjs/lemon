module Data.Task where

import Preload

import Data.Record (class DisjointUnion, class Intersection)

--FIXME: Only allow Basic types in editors


-- Types -----------------------------------------------------------------------

type Message = String

data Task a
  = Task a

data Store a
  = Store a

data User
  = User

instance functorTask :: Functor Task where
  map = undefined
instance applyTask :: Apply Task where
  apply = undefined
instance applicativeTask :: Applicative Task where
  pure = undefined
instance bind :: Bind Task where
  bind = undefined


-- Editors ---------------------------------------------------------------------

enter :: forall a. Message -> Task (Record a)
enter = undefined

edit :: forall a. Message -> Record a -> Task (Record a)
edit = undefined

view :: forall a. Message -> Record a -> Task (Record a)
view = undefined

watch :: forall a. Message -> Store a -> Task { value :: a }
watch = undefined

update :: forall a. Message -> Store a -> Task {}
update = undefined

select :: forall a. Message -> Array a -> Store (Array a) -> Task { values :: Array a }
select = undefined

done :: forall a f. Applicative f => a -> f a
done = pure


-- Stores ----------------------------------------------------------------------

infixl 3 modify as <<-

store :: forall a. a -> Store a
store = undefined

modify :: forall a. Store a -> (a -> a) -> Task Unit
modify = undefined


-- Combinations ----------------------------------------------------------------

infixl 5 and as -&-
infixl 3 or as -|-

and :: forall a b c. DisjointUnion a b c
  => Task (Record a) -> Task (Record b) -> Task (Record c)
and = undefined

or :: forall a b c. Intersection a b c
  => Task (Record a) -> Task (Record b) -> Task (Record c)
or = undefined


-- Options ---------------------------------------------------------------------

infixl 3 pick as -?-

type Check a = a -> Boolean
type Alternative p a =
  { when :: p, then :: Task (Record a) }
type Option a =
  { on :: String, when :: Boolean, then :: Task a }

pick :: forall a b c. Intersection a b c
  => Option (Record a) -> Option (Record b) -> Task (Record c)
pick = undefined

only :: forall a. Option (Record a) -> Task (Record  a)
only = undefined

check :: forall p a b c. Intersection a b c
  => Boolean -> Task (Record a) -> Task (Record b) -> Task (Record c)
check = undefined

match :: forall p a. p -> (p -> Task (Record a)) -> Task (Record a)
match x cases = cases x

with :: forall p. p -> Task {}
with _ = undefined

on :: forall p a b c. Intersection a b c
  => p -> Task (Record a) -> (p -> Task (Record b)) -> p -> Task (Record c)
on = undefined


-- Appointment -----------------------------------------------------------------

infixr 6 appoint as -@-

appoint :: forall a. User -> Task (Record a) -> Task (Record a)
appoint = undefined
