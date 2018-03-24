module Main where

import           Data.Fixed
import           Data.Maybe         (fromJust)
import           Data.Time
import           Data.Time.Calendar

import           Cat
import           Json
import           MyFunctors
import           Person
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

mizzi = Cat "Mizzi" 1 "black"
garfield = Cat "Garfield" 38 "ginger and black"

mainPrintable :: IO ()
mainPrintable = do
  putStrLn "=========== mainPrintable"
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
  putStrLn $ format mizzi
  pprintt mizzi
  putStrLn $ format garfield
  pprintt garfield
  putStrLn "-----"


mainShow :: IO ()
mainShow = do
  putStrLn "=========== mainShow"
  putStrLn $ show 123
  putStrLn $ show 123.45
  print 123.45
  putStrLn $ show False
  putStrLn $ show $ utcTime 2018 3 8 16 38 19
  print $ utcTime 2018 3 8 16 38 19
  now <- getCurrentTime
  putStrLn $ show now
  putStrLn $ show mizzi
  putStrLn $ show garfield
  print garfield


showJson :: Json -> IO()
showJson json = putStrLn str
  where str = show json ++ "\n   --   " ++ stringify json

john = Person "John" True 34 "john@example.com" ["Helen", "Carlie", "Maria"]
dave = Person "Dave" False 45 "dave@eample.com" []

mainJson :: IO ()
mainJson = do
  putStrLn "=========== mainJson"
  showJson $ toJson False
  showJson $ toJson (42 :: Integer)
  showJson $ toJson (42 :: Int)
  showJson $ toJson (42.0 :: Double)
  showJson $ toJson (42.0 :: Float)
  showJson $ toJson "a String"
  showJson $ toJson $ Just "a String"
  showJson $ toJson (Nothing :: Maybe String)
  showJson $ toJson john
  showJson $ toJson dave
  showJson $ toJson $ Just dave
  showJson $ toJson (Nothing :: Maybe Person)


mainEq :: IO ()
mainEq = do
  putStrLn "=========== mainEq"
  putStrLn "--> Scala programming error, Haskell compile error ..."
  -- List(1, 2, 3).map(Option(_)).filter(_ == 1)    ==> Scala: programming error, compiler gives just a warning
  -- filter (== 1) $ map Just [1,2,3]               ==> Haskell: the code doesn't compile
  print $ filter (== Just 1) $ map Just [1,2,3]

  putStrLn "--> Comparing Integers ..."
  -- 123 == "xyz"                                   ==> Scala: programming error, compiler gives just a warning
  -- 123 == "xyz"                                   ==> Haskell: the code doesn't compile
  print $ 123 == 123
  print $ 123 /= 123
  print $ 123 == 234
  print $ 123 /= 234

  putStrLn "--> Comparing Maybes ..."
  print $ Just 1 == Just 1
  print $ Just 1 == Just 2
  print $ Just 1 == Nothing
  print $ Just 1 /= Nothing

  putStrLn "--> Comparing Cats ..."
  print $ mizzi == mizzi
  print $ mizzi == garfield

  putStrLn "--> Comparing Maybe[Cat] ..."
  print $ Just mizzi == Just mizzi
  print $ Just mizzi == Just garfield


myList = [1, 2, 3, 4, 5]
mySome = Just 5
myNone :: Maybe Integer
myNone = Nothing
myId = Identity 5
myPair = Pair "foo" 5
myPair' = Pair' 3 5

mainFunctors :: IO ()
mainFunctors = do
  putStrLn "=========== mainFunctors"

  putStrLn "--- mapping List"
  print myList
  print $ map (\x -> x * 2) myList -- map function of List
  print $ map (*2) myList
  print $ fmap (\x -> x * 2) myList -- fmap function of List functor
  print $ fmap (*2) myList
  print $ (*2) <$> myList -- <$> is an alias for fmap (infix operator)
  print $ fmap (^2) myList
  print $ (^2) <$> myList

  putStrLn "--- mapping Maybe"
  putStrLn "Maybe doesn't provide a map function, use fmap instead"
  print mySome
  print myNone
  print $ fmap (*2) mySome -- fmap function of Maybe functor
  print $ fmap (*2) myNone
  print $ (^2) <$> mySome -- <$> is an alias for fmap (infix operator)
  print $ (^2) <$> myNone

  putStrLn "--- mapping Identity"
  print myId
  print $ fmap (\x -> x * 2) myId
  print $ fmap (*2) myId
  print $ (*2) <$> myId
  print $ fmap (^2) myId
  print $ (^2) <$> myId

  putStrLn "--- mapping Pair"
  print myPair
  print $ fmap (*2) myPair
  print $ (^2) <$> myPair

  putStrLn "--- mapping Pair'"
  print myPair'
  print $ fmap (*2) myPair'
  print $ (^2) <$> myPair'


main :: IO ()
main = do
  mainPrintable
  mainShow
  mainJson
  mainEq
  mainFunctors
  putStrLn "==========="
