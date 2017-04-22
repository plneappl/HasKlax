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
           | otherwise                         = Just (toEnum i :: a)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal = replaceNth' n (const newVal)

replaceNth' :: Int -> (a -> a) -> [a] -> [a]
replaceNth' n f (x:xs)
     | n == 0 = f x : xs
     | otherwise = x : replaceNth' (n-1) f xs
replaceNth' _ _ [] = []


onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (a, b) = (f a, b)
onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (a, b) = (a, f b)

fst3 (a, b, c) = a
snd3 (a, b, c) = b
trd3 (a, b, c) = c

prefixes, suffixes :: [a] -> [[a]]
suffixes xs@(y:ys) = xs:prefixes ys 
suffixes [] = [[]]
prefixes = suffixes . reverse

indexed :: [[a]] -> [(Int, [(Int, a)])]
indexed  = zip [0..] . map (zip [0..]) 

stripes :: [(Int, [(Int, a)])] -> [[(Int, Int, a)]]
stripes idxbs@((x, (y, b):rows):cols) = let 
  othersOneDown = map (onSnd tail) cols
  thisStripe = oneStripe idxbs
  -- all stripes in the lower right corner
  -- already visited by others
  --otherStripes1 = stripes othersOneDown
  -- all stripes one to the right
  otherStripes2 = stripes cols
  -- all stripes one down
  otherStripes3 = stripes ((x, rows):othersOneDown) in
  thisStripe : otherStripes2 ++ otherStripes3
stripes _ = []

oneStripe :: [(Int, [(Int, a)])] -> [(Int, Int, a)]
oneStripe ((x, (y, b):rows):cols) = (x, y, b) : oneStripe (map (onSnd tail) cols)
oneStripe _ = []
