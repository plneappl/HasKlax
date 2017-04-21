module Block where

import System.Random

data BType = BRed | BBlue | BGreen deriving (Eq, Ord, Show, Enum, Bounded)

data Block = B BType deriving (Eq, Ord, Show, Bounded)
instance Enum Block where
  toEnum i = B (toEnum i)
  fromEnum (B b) = fromEnum b




