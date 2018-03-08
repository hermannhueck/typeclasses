{-# LANGUAGE FlexibleInstances #-}

module Printable where

import           Data.Time

-- the type class 'Printable'
--
class Printable a where
    format :: a -> String


-- some type class instances
--
instance Printable String where
    format s = s

instance Printable Int where
    format = show

instance Printable Double where
    format = show

instance Printable UTCTime where
    format = show
