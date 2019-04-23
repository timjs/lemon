module Data.Task where

import Preload (Unit, class Functor, class Apply, class Applicative, class Bind, class Monad, pure, undefined)

import Data.Record (class DisjointUnion, class Intersection)

--FIXME: Only allow Basic types in editors


-- Types -----------------------------------------------------------------------

type Message = String
type Check a = a -> Boolean

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
instance bindTask :: Bind Task where
  bind = undefined
instance monadTask :: Monad Task


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

done :: forall a. Record a -> Task (Record a)
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


-- Continuations ---------------------------------------------------------------

infixl 3 xor as -|

type Continuation r a = { then :: Task a | r }
type Branch a = Continuation ( when :: Boolean ) a
type Option a = Continuation ( on :: String, when :: Boolean ) a

only :: forall a r. Continuation r (Record a) -> Task (Record a)
only = undefined

xor :: forall a b c r. Intersection a b c
  => Continuation r (Record a) -> Continuation r (Record b) -> Continuation r (Record c)
xor = undefined

when :: forall a. Boolean -> Task (Record a) -> Branch (Record a)
when = undefined

on :: forall a. String -> Boolean -> Task (Record a) -> Option (Record a)
on = undefined

check :: forall a b c. Intersection a b c
  => Boolean -> Task (Record a) -> Task (Record b) -> Task (Record c)
check = undefined


-- Appointment -----------------------------------------------------------------

infixr 6 appoint as -@-

appoint :: forall a. User -> Task (Record a) -> Task (Record a)
appoint = undefined
