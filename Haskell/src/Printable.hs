{-# LANGUAGE FlexibleInstances #-}

module Printable where

import           Data.List
import           Data.Time

-- the type class 'Printable'
--
class Printable a where

    stringify :: a -> String

    pprint :: a -> IO ()
    pprint x = putStrLn $ stringify x


-- some type class instances for common types
--
instance Printable String where
    stringify = intersperse '-'

instance Printable Int where
    stringify = show

instance Printable Double where
    stringify = show

instance Printable UTCTime where
    stringify time = "The exact date is: " ++ formatTime defaultTimeLocale "%F, %T (%Z)" time
