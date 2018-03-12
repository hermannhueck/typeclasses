module Cat where

import           Printable


-- domain classes
--
data Cat = Cat
  { name  :: String
  , age   :: Int
  , color :: String
  } deriving (Eq, Show)


-- type class instances for doamin classes
--
instance Printable Cat where
    format cat = "Cat {name=" ++ name cat ++ ", age=" ++ show (age cat) ++ ", color=" ++ color cat ++ "}"
