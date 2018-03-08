module Main where

import           Data.Fixed
import           Data.Maybe         (fromJust)
import           Data.Time
import           Data.Time.Calendar
import           Printable

dayOf :: Integer -> Int -> Int -> Day
dayOf year month day = fromJust $ fromGregorianValid year month day

timeOf :: Int -> Int -> Int -> TimeOfDay
timeOf hour min sec = TimeOfDay hour min fixed
  where fixed = MkFixed $ toInteger $ sec * 10^12

utcTimeOf :: Day -> TimeOfDay -> UTCTime
utcTimeOf day tod = UTCTime day (timeOfDayToTime tod)

utcTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
utcTime year month day hour min sec = utcTimeOf d t
  where
    d = fromJust $ fromGregorianValid year month day
    t = timeOf hour min sec

main :: IO ()
main = do
  putStrLn "-----"
  putStrLn "Cats are meeting here!"
  putStrLn $ format "Cats are meeting here!"
  putStrLn $ format (2 :: Int)
  putStrLn $ format (2 :: Double)
  putStrLn $ format $ utcTime 2018 3 8 16 38 19
  now <- getCurrentTime
  putStrLn $ format now
  putStrLn "-----"
