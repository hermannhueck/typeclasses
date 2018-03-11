module Main where

import           Data.Fixed
import           Data.Maybe         (fromJust)
import           Data.Time
import           Data.Time.Calendar

import           Cat
import           Printable

import           Json
import           Person

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

mainPrintable :: IO ()
mainPrintable = do
  putStrLn "==========="
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

showJson :: Json -> IO()
showJson json = putStrLn str
  where str = show json ++ "\n   --   " ++ stringify json

john = Person "John" True 34 "john@example.com" ["Helen", "Carlie", "Maria"]
dave = Person "Dave" False 45 "dave@eample.com" []

mainJson :: IO ()
mainJson = do
  putStrLn "==========="
  showJson $ toJson False
  showJson $ toJson (42 :: Integer)
  showJson $ toJson (42 :: Int)
  showJson $ toJson (42.0 :: Double)
  showJson $ toJson (42.0 :: Float)
  showJson $ toJson "a String"
  showJson $ toJson $ Just "a String"
  showJson $ toJson $ (Nothing :: Maybe String)
  showJson $ toJson john
  showJson $ toJson dave
  showJson $ toJson $ Just dave
  showJson $ toJson $ (Nothing :: Maybe Person)

main :: IO ()
main = do
  mainPrintable
  mainJson
