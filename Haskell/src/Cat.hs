module Cat where

import           Printable

data Cat = Cat
  { name  :: String
  , age   :: Int
  , color :: String
  }

instance Printable Cat where
    format cat = "Cat {name=" ++ name cat ++ ", age=" ++ show (age cat) ++ ", color=" ++ color cat ++ "}"
