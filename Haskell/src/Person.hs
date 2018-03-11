module Person where

import qualified Data.Map as M
import           Json

-- domain classes
--
data Person = Person
  { name     :: String
  , married  :: Bool
  , age      :: Int
  , email    :: String
  , children :: [String]
  }

-- type class instances for doamin classes
--
instance JsonWriter Person where
  toJson person = JsObject (M.fromList tuples)
    where
      tuples :: [(String, Json)]
      tuples =
        [ ("name", toJson $ name person)
        , ("married", toJson $ married person)
        , ("age", toJson $ age person)
        , ("email", toJson $ email person)
        , ("children", toJson $ children person)
        ]
