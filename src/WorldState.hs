module WorldState where

import System.Random
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Tuple
import Block
import Config

initialWorld :: StdGen -> WorldState
initialWorld r = Running { 
    _rand = r
  , lanes = replicate lanesCount emptyLane
  , stackingArea = replicate stackHeight emptyStackLane
  , timeElapsed = 0
  , lastBlock = 0
  , paddlePosition = 0
  , paddleStack = [] } where
  emptyLane = replicate laneLength Nothing
  emptyStackLane = replicate lanesCount Nothing

data WorldState = Running { 
    _rand :: StdGen 
  , lanes :: [Lane]
  , stackingArea :: [Lane]
  , timeElapsed :: Float
  , lastBlock :: Int
  , paddlePosition :: Int
  , paddleStack :: [Block]
  }

type Lane = [Maybe Block]

type Position = (Int, Int)

klaxAchieved :: WorldState -> Maybe [Position]
klaxAchieved Running {stackingArea = sa} = verticalKlax sa <|> horizontalKlax sa <|> diagonalKlax1 <|> diagonalKlax2 where
  indexed :: [[a]] -> [(Int, [(Int, a)])]
  indexed  = zip [0..] . map (zip [0..]) 

  verticalKlax = msum . map vKlax . indexed 
  vKlax (x, bs) = do 
    guard $ length same >= klaxMinLength
    guard $ isJust $ snd $ head same
    return $ map (\(y, b) -> (x, y)) same where
    grouped = groupBy (\x y -> snd x == snd y) bs
    same = minimumBy (\x y -> compare (length x) (length y)) grouped

  horizontalKlax = fmap (fmap swap) . verticalKlax . transpose

  diagonalKlax1 = undefined
  diagonalKlax2 = undefined                                     


