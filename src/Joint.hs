{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Joint where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Distributive

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) f g x = f (g x)

-- merge definitions for custom language
newtype (:<:) t u a = (:<:) {runTU :: t (u a)}
newtype (:>:) u t a = (:>:) {runUT :: u (t a)}
infixr 1 :<:, :>:

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(f <.> g) x = f <$> g x

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)
(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<**>) = (<*>) . fmap (<*>)

class Transformer f g where
  lift :: f a -> g a
instance Transformer f f where
  lift = id
instance MonadIO f => Transformer IO f where
  lift = liftIO

--instance (Transformer f g, Transformer g h) => Transformer f h where
--  lift = lift . lift

instance (Functor t, Functor u) => Functor (t :<: u) where
  fmap f ((:<:) x) = (:<:) $ f <$$> x
instance (Applicative t, Applicative u) => Applicative (t :<: u) where
  pure = (:<:) . pure . pure
  (:<:) f <*> (:<:) x = (:<:) $ f <**> x
instance (Distributive t, Monad t, Monad u) => Monad (t :<: u) where
  (:<:) x >>= f = (:<:) $ x >>= (join <.> collect (runTU . f))

instance (Transformer f u, Applicative t) => Transformer f (t :<: u) where
  lift = (:<:) . pure . lift
instance (Functor t, Applicative u) => Transformer t (t :<: u) where
  lift = (:<:) . fmap pure

instance (Functor u, Functor t) => Functor (u :>: t) where
  fmap f ((:>:) x) = (:>:) $ f <$$> x
instance (Applicative u, Applicative t) => Applicative (u :>: t) where
  pure = (:>:) . pure . pure
  (:>:) f <*> (:>:) x = (:>:) $ f <**> x
instance (Traversable t, Monad t, Monad u) => Monad (u :>: t) where
  (:>:) x >>= f = (:>:) $ x >>= (join <.> traverse (runUT . f))

instance (Transformer f t, Applicative u) => Transformer f (u :>: t) where
  lift = (:>:) . pure . lift
instance (Functor u, Applicative t) => Transformer u (u :>: t) where
  lift = (:>:) . fmap pure

class Functor t => Adjunction t u where
  leftAdjunct :: (t a -> b) -> a -> u b
  rightAdjunct :: (a -> u b) -> t a -> b
  unit :: a -> u (t a)
  unit = leftAdjunct id
  counit :: t (u a) -> a
  counit = rightAdjunct id

instance Adjunction ((,) s) ((->) s) where
  leftAdjunct f a s = f (s, a)
  rightAdjunct f (s, a) = f a s

-- TODO: TUT
