{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Json where

import           Data.List (intercalate, intersperse)
import qualified Data.Map  as M
import           GHC.Float (float2Double)

data Json
  = JsNull
  | JsBoolean Bool
  | JsNumber Double
  | JsString String
  | JsArray [Json]
  | JsObject (M.Map String Json)
  deriving (Eq, Show)

stringify :: Json -> String
stringify JsNull           = "null"
stringify (JsBoolean bool) = show bool
stringify (JsNumber num)   = show num
stringify (JsString str)   = str
stringify (JsArray elems) = "[" ++ (intercalate ", " $ map stringify elems) ++ "]"
stringify (JsObject bindings) =
  let mapStringString = M.map stringify bindings :: M.Map String String
      tuples = M.toList mapStringString :: [(String, String)]
      strings = map (\(x, y) -> x ++ ": " ++ y) tuples :: [String]
      oneString = intercalate ", " strings :: String
  in "{" ++ oneString ++ "}"

-- the type class
class JsonWriter a where
  toJson :: a -> Json

-- type class instances
--
instance {-# OVERLAPPING #-} JsonWriter String where
  toJson = JsString

instance JsonWriter Bool where
  toJson = JsBoolean

instance JsonWriter Double where
  toJson = JsNumber

instance JsonWriter Float where
  toJson = JsNumber . float2Double

instance JsonWriter Integer where
  toJson = JsNumber . fromRational . fromInteger

instance JsonWriter Int where
  toJson = JsNumber . fromRational . fromIntegral

instance JsonWriter a => JsonWriter (Maybe a) where
  toJson Nothing  = JsNull
  toJson (Just a) = toJson a

instance {-# OVERLAPPABLE #-} JsonWriter a => JsonWriter [a] where
  toJson = JsArray . map toJson

instance JsonWriter a => JsonWriter (M.Map String a) where
  toJson = JsObject . M.map toJson
