module Events where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import WorldState
import Config
import Lib
import Debug.Trace
import Data.Maybe

type WorldMod m = WorldState -> m WorldState

eventHandler :: Event -> WorldMod IO
eventHandler (EventKey (Char k) Down _ _) w@Running {paddlePosition = pos} = let
  pos' = case k of
          'a' -> pos - 1
          'd' -> pos + 1
          _   -> pos
  pos'' = max 0 (min (lanesCount - 1) pos') in
  return $ dropTopOfStack k $ w {paddlePosition = pos''}
eventHandler _ w = return w

dropTopOfStack :: Char -> WorldState -> WorldState
dropTopOfStack 's' w@Running{paddlePosition = pos, paddleStack = ps, stackingArea = sa} =  
  if null ps then w else
  let stackCol = sa !! pos in
  if isJust $ head stackCol then w else let 
  block = Just $ head ps 
  (emty, blcks) = break isJust stackCol
  stackCol' = (tail emty) ++ [block] ++ blcks in 
  w {stackingArea = replaceNth pos stackCol' sa, paddleStack = tail ps}
dropTopOfStack  _  w = w


onTick :: Float -> WorldMod IO
onTick timePassed world@Running { timeElapsed = timeOld } = let 
  totalTimePassed = timePassed + timeOld
  newTimePassed = totalTimePassed `mod'` tickLength in
  return (run world totalTimePassed) { timeElapsed = newTimePassed } where
  run :: WorldState -> Float -> WorldState
  run w@Running{lastBlock = lb, lanes = l, paddlePosition = pos, paddleStack = ps} t = if t < tickLength then w else let
    (w', newBlocks) = if lb == blockEvery - 1 then generateBlocks w else (w, replicate lanesCount Nothing)
    lb' = (lb + 1) `mod` blockEvery
    (lanes', stackTop) = tickLanes newBlocks l 
    psBlock = stackTop !! pos
    ps' = case psBlock of 
      Just b | length ps < paddleStackHeight -> b : ps
      _                                      -> ps in
    run (w' {lanes = lanes', lastBlock = lb', paddleStack = ps'}) (t - tickLength)

tickLanes :: Lane -> [Lane] -> ([Lane], Lane)
tickLanes lnew ls = (zipWith (\b lane -> tail lane ++ [b]) lnew ls, map head ls)

generateBlocks :: WorldState -> (WorldState, Lane)
generateBlocks w@Running {_rand = r} = let
  (newBlock, r') = random r
  pos :: Int
  (pos, r'') = randomR (0, lanesCount - 1) r'
  l = take lanesCount $ replicate pos Nothing ++ [Just newBlock] ++ repeat Nothing in 
  (w{_rand = r''}, l)
