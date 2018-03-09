module Domain where

import           Printable


-- domain classes
--
data Cat = Cat
  { name  :: String
  , age   :: Int
  , color :: String
  }


-- type class instances for doamin classes
--
instance Printable Cat where
    format cat = "Cat {name=" ++ name cat ++ ", age=" ++ show (age cat) ++ ", color=" ++ color cat ++ "}"
