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

import Prelude (class Applicative, class Monad, pure)

import Control.Category (class Category, identity)
import Control.Comonad (class Comonad, extract)
import Control.Plus (class Plus, empty)
import Control.Semigroupoid (class Semigroupoid, compose, (<<<), (>>>))
import Data.Function (apply) as Function
import Data.Identity (Identity(Identity))
-- import Data.Iso (Iso(Iso))
import Data.Leibniz (Leibniz, runLeibniz)
import Data.Machine.Mealy (MealyT, Step(Emit, Halt), runMealyT)
import Data.Newtype (un)
import Data.Profunctor.Star (Star(Star))
-- import Record.Builder (Builder, build)
import Record.Builder (Builder)

class Evaluable (p :: Type -> Type -> Type) (a :: Type) (b :: Type)
instance evaluableBuilder :: Evaluable Builder (Record a) (Record b)
instance evaluableFn :: Evaluable (->) a b
instance evaluableLeibniz :: Evaluable Leibniz a b

instance evaluableStar :: Monad f => Evaluable (Star f) (f a) a

class Category p <= ClosedMonoidal p where
  eval :: forall a b. Evaluable p a b => p a b -> a -> b

-- instance closedMonoidalBuilder :: ClosedMonoidal Builder where
--   eval builder record = build builder record

instance closedMonoidalFn :: ClosedMonoidal (->) where
  eval = Function.apply

instance closedMonoidalLeibniz :: ClosedMonoidal Leibniz where
  eval leibniz = un Identity <<< runLeibniz leibniz <<< Identity

instance closedMonoidalStar
  :: ( Comonad f
     , Monad f
     )
  => ClosedMonoidal (Star f)
  where
  eval (Star f) x = extract (f x)

-- Also the inverse.
-- instance closedMonoidalIso :: ClosedMonoidal Iso where
--   eval (Iso f g) _ = g

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
