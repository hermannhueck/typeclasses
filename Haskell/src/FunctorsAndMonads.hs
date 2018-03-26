module FunctorsAndMonads where


newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  -- pure :: a -> f a
  pure = Identity
  -- (<*>) :: f(a -> b) -> f a -> f b
  (<*>) (Identity f) (Identity x) = Identity $ f x
  -- (<*>) (Identity f) = fmap f -- defines (<*>) in terms of fmap

instance Monad Identity where
  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) (Identity x) f = f x


data Pair a b = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Pair x y) = Pair x (f y)

instance Monoid a => Applicative (Pair a) where
  -- pure :: a -> f a
  pure = Pair mempty
  -- (<*>) :: f(a -> b) -> f a -> f b
  (<*>) (Pair x f) (Pair x' y) = Pair (mappend x x') (f y)

instance Monoid a => Monad (Pair a) where
  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) (Pair x y) f = Pair (mappend x x') y'
    where Pair x' y' = f y


data Pair2 a = Pair2 a a
  deriving (Eq, Show)

instance Functor Pair2 where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Pair2 x y) = Pair2 (f x) (f y)

instance Applicative Pair2 where
  -- pure :: a -> f a
  pure x = Pair2 x x
  -- (<*>) :: f(a -> b) -> f a -> f b
  Pair2 f g <*> Pair2 x y = Pair2 (f x) (g y)

{-
-- Don't know how to write a Monad for Pair2

instance Monad Pair2 where
  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) (Pair2 x y) f = undefined -- Pair2 x' y'
    where
      Pair2 x' y' = f x
      Pair2 x'' y'' = f y
-}
