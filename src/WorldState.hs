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
import Lib
import Debug.Trace

initialWorld :: StdGen -> WorldState
initialWorld r = Running { 
    _rand = r
  , lanes = replicate lanesCount emptyLane
  , stackingArea = replicate stackHeight emptyStackLane
  , timeElapsed = 0
  , lastBlock = 0
  , lastBlockMovement = 0
  , paddlePosition = 0
  , paddleStack = []
  , paddleMovement = MStay
  , moveOnce = MStay } where
  emptyLane = replicate laneLength Nothing
  emptyStackLane = replicate lanesCount Nothing

data WorldState = Running { 
    _rand :: StdGen 
  , lanes :: [Lane]
  , stackingArea :: [Lane]
  , timeElapsed :: Float
  , lastBlock :: Int
  , lastBlockMovement :: Int
  , paddlePosition :: Int
  , paddleStack :: [Block]
  , paddleMovement :: Direction
  , moveOnce :: Direction
  }

data Direction = MStay | MLeft | MRight deriving (Eq, Ord, Show)

type Lane = [Maybe Block]

type Position = (Int, Int)

klaxAchieved :: WorldState -> Maybe [Position]
klaxAchieved Running {stackingArea = sa} = diagonalKlax1 sa <|> diagonalKlax2 sa <|> verticalKlax sa <|> horizontalKlax sa where
  verticalKlax, horizontalKlax, diagonalKlax1, diagonalKlax2 :: [Lane] -> Maybe [Position]
  verticalKlax = msum . map vKlax . indexed 
  -- assume klaxes are at least half the board width. then there can only be one klax, so if there is one, it has to be the one having the most same blocks in a run.
  -- if the most blocks are Nothing, then there can't be a klax of blocks.
  vKlax (x, bs) = do 
    guard $ length same >= klaxMinLength
    guard $ isJust $ snd $ head same
    trace (show grouped) $ trace (show same) $ return $ map (\(y, b) -> (x, y)) same where
    grouped = groupBy (\x y -> snd x == snd y) bs
    same = maximumBy (\x y -> compare (length x) (length y)) grouped

  horizontalKlax = fmap (fmap swap) . verticalKlax . transpose

  -- top left --> bottom right
  diagonalKlax1 as = let 
    strps = stripes (indexed as)
    strpsSame = filter (\s -> length (groupBy (\ (_, _, b1) (_, _, b2) -> b1 == b2) s) == 1) strps 
    strpLenOk = filter (\s -> length s >= klaxMinLength) strpsSame 
    strpsJust = filter (\((_, _, b):_) -> isJust b) strpLenOk in
    fmap (\(x, y, _) -> (x, y)) <$> listToMaybe strpsJust

  diagonalKlax2 = fmap (fmap (\(x, y) -> (lanesCount - x - 1, y))) . diagonalKlax1 . reverse                                   


