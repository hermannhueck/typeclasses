module MyFunctors where


newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Identity x) = Identity $ f x


data Pair a b = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Pair x y) = Pair x (f y)


data Pair' a = Pair' a a
  deriving (Eq, Show)

instance Functor Pair' where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Pair' x y) = Pair' (f x) (f y)
