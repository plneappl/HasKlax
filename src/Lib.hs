{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module Lib where

import System.Random

someFunc = putStrLn "someFunc"

mod' :: Float -> Float -> Float
mod' a m = let 
  d = fromIntegral $ truncate (a / m) in
  a - (d * m)

instance {-# OVERLAPPABLE #-} (Enum a, Bounded a) => Random a where
  randomR (mn, mx) r = let 
    (res, r') = randomR (fromEnum mn, fromEnum mx) r in
    (toEnum res, r')
  random = randomR (minBound, maxBound)

instance (Bounded a) => Bounded (Maybe a) where
  minBound = Nothing
  maxBound = Just maxBound

instance forall a. (Enum a, Bounded a) => Enum (Maybe a) where
  fromEnum Nothing  = (fromEnum (minBound :: a)) - 1
  fromEnum (Just x) =  fromEnum x
  toEnum i | i <= fromEnum (minBound :: a) - 1 = Nothing
           | otherwise                         = Just (toEnum $ i :: a)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs
replaceNth _ _ [] = []
