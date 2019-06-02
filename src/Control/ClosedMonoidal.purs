module Control.ClosedMonoidal
  ( class ClosedMonoidal
  , class Evaluable
  , const
  , eval
  , evalFlipped
  , on
  , ($)
  , (#)
  , module Control.Category
  , module Control.Semigroupoid
  ) where

import Prelude (class Applicative, pure)

import Control.Category (class Category, identity)
import Control.Semigroupoid (class Semigroupoid, compose, (<<<), (>>>))
import Data.Function (apply) as Function
-- import Record.Builder (Builder, build)
import Record.Builder (Builder)

class Evaluable (p :: Type -> Type -> Type) (a :: Type) (b :: Type)
instance evaluableBuilder :: Evaluable Builder (Record a) (Record b)
instance evaluableFn :: Evaluable (->) a b

class Category p <= ClosedMonoidal p where
  eval :: forall a b. Evaluable p a b => p a b -> a -> b

-- instance closedMonoidalBuilder :: ClosedMonoidal Builder where
--   eval builder record = build builder record

instance closedMonoidalFn :: ClosedMonoidal (->) where
  eval = Function.apply

infixr 0 eval as $

const :: forall a b p. ClosedMonoidal p => Applicative (p b) => a -> p b a
const a = pure a

evalFlipped
  :: forall a b p
   . ClosedMonoidal p
  => Evaluable p a b
  => a
  -> p a b
  -> b
evalFlipped x f = eval f x

infixl 1 evalFlipped as #

on
  :: forall a b c p
   . ClosedMonoidal p
  => Evaluable p a b
  => (b -> b -> c)
  -> p a b
  -> a
  -> a
  -> c
on f g x y = f (eval g x) (eval g y)

-- -- ==============================================================================
-- -- class Profunctor p where
-- --   dimap :: forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d
-- -- package 'profunctor'
-- -- | Lift a pure function into any `Profunctor` which is also a `Category`.
-- arr :: forall a b p. Category p => Profunctor p => (a -> b) -> p a b
-- arr f = rmap f identity
-- -- ==============================================================================
-- type NaturalTransformation1 f g = forall a b. f a b -> g a b
-- class Functor f <= Apply f where
--   apply :: forall a b. f (a -> b) -> f a -> f b
--   apply :: forall a b. f (a, b) -> (f a, f b)
--   eval  :: forall a b. ((a, b), a) -> b
-- apply :: forall a b p. ClosedMonoidal p => f (p a b) -> p (f a) (f b)
-- -- ==============================================================================
