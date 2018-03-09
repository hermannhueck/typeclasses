module Main where

import           Data.Fixed
import           Data.Maybe         (fromJust)
import           Data.Time
import           Data.Time.Calendar

import           Domain
import           Printable

dayOf :: Integer -> Int -> Int -> Day
dayOf year month day = fromJust $ fromGregorianValid year month day

timeOf :: Int -> Int -> Int -> TimeOfDay
timeOf hour min sec = TimeOfDay hour min $ intToPico sec

intToPico :: Int -> Pico
intToPico value = MkFixed $ toInteger $ value * 10 ^ 12

utcTimeOf :: Day -> TimeOfDay -> UTCTime
utcTimeOf day tod = UTCTime day (timeOfDayToTime tod)

utcTime :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
utcTime year month day hour min sec = utcTimeOf d t
  where
    d = fromJust $ fromGregorianValid year month day
    t = timeOf hour min sec

main :: IO ()
main = do
  putStrLn "----- formatting Strings ..."
  putStrLn "Cats are meeting here!"
  putStrLn $ format "Cats are meeting here!"
  pprintt "Cats are meeting here!"
  putStrLn "----- formatting Numbers ..."
  putStrLn $ format (2 :: Int)
  pprintt (2 :: Int)
  putStrLn $ format (2 :: Double)
  pprintt (2 :: Double)
  putStrLn "----- formatting UTCTimes ..."
  putStrLn $ format $ utcTime 2018 3 8 16 38 19
  pprintt $ utcTime 2018 3 8 16 38 19
  now <- getCurrentTime
  pprintt now
  putStrLn "----- formatting Cats ..."
  let mizzi = Cat "Mizzi" 1 "black"
      garfield = Cat "Garfield" 38 "ginger and black"
  putStrLn $ format mizzi
  pprintt mizzi
  putStrLn $ format garfield
  pprintt garfield
  putStrLn "-----"
