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
eventHandler (EventKey (Char k) Down _ _) w@Running {paddleMovement = movOld, moveOnce = movO} = let
  movement = case k of
    'a' -> MLeft
    'd' -> MRight
    _   -> movOld in
  return $ dropTopOfStack k $ w { paddleMovement = movement, moveOnce = if movOld == movement then movO else movement }
eventHandler (EventKey (Char k) Up _ _) w@Running {paddleMovement = mov} = let
  movement = case (k, mov) of
    ('a', MLeft)  -> MStay
    ('d', MRight) -> MStay
    _   -> mov in
  return $ w {paddleMovement = movement}
eventHandler _ w = return w

dropTopOfStack :: Char -> WorldState -> WorldState
dropTopOfStack 's' w@Running{paddlePosition = pos, paddleStack = ps, stackingArea = sa} =  
  if null ps then w else
  let stackCol = sa !! pos in
  if isJust $ head stackCol then w else let 
  block = Just $ head ps 
  (emty, blcks) = break isJust stackCol
  stackCol' = tail emty ++ [block] ++ blcks in 
  w {stackingArea = replaceNth pos stackCol' sa, paddleStack = tail ps}
dropTopOfStack  _  w = w


onTick :: Float -> WorldMod IO
onTick timePassed world@Running { timeElapsed = timeOld } = let 
  totalTimePassed = timePassed + timeOld
  newTimePassed = totalTimePassed `mod'` tickLength in
  return (run world totalTimePassed) { timeElapsed = newTimePassed } where
  run :: WorldState -> Float -> WorldState
  run w@Running{
    lastBlock = lb, 
    lastBlockMovement = lbm, 
    lanes = l, 
    paddlePosition = pos, 
    paddleStack = ps, 
    paddleMovement = mov,
    moveOnce = movO} t = if t < tickLength then w else let
    mov' = if movO == MStay then mov else movO
    pos' = case mov' of 
      MLeft  -> max 0 (pos - 1)
      MRight -> min (lanesCount - 1) (pos + 1)
      MStay  -> pos
    w1 = w { paddlePosition = pos', moveOnce = MStay } 
    klax = klaxAchieved w1 in 

    case klax of 
      Just klax' -> compressStack $ deleteKlax klax' w 
      _ -> let 
    
        (w2, newBlocks) = if lb == blockEvery - 1 then generateBlocks w1 else (w1, replicate lanesCount Nothing)
        w3 = w2 { lastBlock = (lb + 1) `mod` blockEvery }
        (lanes', stackTop) = if lbm == blockSpeed - 1 then tickLanes newBlocks l else (l, replicate lanesCount Nothing)
        w4 = w3 { lanes = lanes' }
        w5 = w4 { lastBlockMovement = (lbm + 1) `mod` blockSpeed }
        psBlock = stackTop !! pos
        ps' = case psBlock of 
          Just b | length ps < paddleStackHeight -> b : ps
          _                                      -> ps in
        run (w5 {paddleStack = ps'}) (t - tickLength)

tickLanes :: Lane -> [Lane] -> ([Lane], Lane)
tickLanes lnew ls = (zipWith (\b lane -> tail lane ++ [b]) lnew ls, map head ls)

generateBlocks :: WorldState -> (WorldState, Lane)
generateBlocks w@Running {_rand = r} = let
  (newBlock, r') = random r
  pos :: Int
  (pos, r'') = randomR (0, lanesCount - 1) r'
  l = take lanesCount $ replicate pos Nothing ++ [Just newBlock] ++ repeat Nothing in 
  (w{_rand = r''}, l)

deleteKlax :: [Position] -> WorldState -> WorldState
deleteKlax [] w = w
deleteKlax ((x, y):pos) w@Running{stackingArea = sa} = deleteKlax pos (w {stackingArea = replaceNth' x (replaceNth y Nothing) sa})

compressStack :: WorldState -> WorldState
compressStack w@Running{ stackingArea = sa } = w { stackingArea = map compress sa } where
  compress xs = let 
    (space1, rest1) = break isJust xs
    (blocks1, rest2) = break isNothing rest1
    (space2, blocks2) = break isJust rest2 in
    space1 ++ space2 ++ blocks1 ++ blocks2
