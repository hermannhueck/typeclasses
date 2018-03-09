{-# LANGUAGE FlexibleInstances #-}

module Printable where

import           Data.List
import           Data.Time

-- the type class 'Printable'
--
class Printable a where
    format :: a -> String
    pprintt :: a -> IO ()
    pprintt x = putStrLn $ format x


-- some type class instances for common types
--
instance Printable String where
    format = intersperse '-'

instance Printable Int where
    format = show

instance Printable Double where
    format = show

instance Printable UTCTime where
    format time = "The exact date is: " ++ formatTime defaultTimeLocale "%F, %T (%Z)" time
