module Data.Task where

import Preload

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

enter :: forall a.
  Message -> {} -> Task { value :: a }
enter = undefined

edit :: forall a.
  Message -> { value :: a } ->Task { value :: a }
edit = undefined

view :: forall a.
  Message -> { value :: a } -> Task { value :: a }
view = undefined

watch :: forall a.
  Message -> Store a -> {} -> Task { value :: a }
watch = undefined

update :: forall a.
  Message -> Store a -> {} -> Task {}
update = undefined

select :: forall a.
  Message -> Array a -> Store (Array a) -> {} -> Task { value :: Array a }
select = undefined


-- Stores ----------------------------------------------------------------------

infixl 3 modify as <<-

store :: forall a.
  a -> Store a
store = undefined

modify :: forall a.
  Store a -> (a -> a) -> Task Unit
modify = undefined


-- Combinators -----------------------------------------------------------------

infixl 1 andThen as >>+
infixl 1 andNext as >>?

andThen :: forall a b.
  Task a -> Array { pred :: a -> Boolean, cont :: a -> Task b } -> Task b
andThen = undefined

andNext :: forall a b.
  Task a -> Array { name :: String, pred :: a -> Boolean, cont :: a -> Task b } -> Task b
andNext = undefined

allOf2 :: forall a b.
  { _1 :: Task a, _2 :: Task b } -> Task { _1 :: a, _2 :: b }
allOf2 = undefined

anyOf :: forall a. -- Intersection over a's
  Array (Task a) -> Task a
anyOf = undefined

oneOf :: forall a.
  Array { name :: String, pred :: Boolean, cont :: Task a } -> Task a
oneOf = undefined


-- Helpers ---------------------------------------------------------------------

always :: forall a. a -> Boolean
always = const true

undefined :: forall a. a
undefined = unsafeCoerce unit
