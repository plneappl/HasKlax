module BlockDrawing where

import Graphics.Gloss
import Block
import Config
import WorldState

blockWidth, blockHeightLane :: Float
blockWidth        = 1 / fromIntegral lanesCount
blockHeightLane   = 1 / fromIntegral laneLength
blockHeightStack  = 1 / fromIntegral stackHeight
blockHeightPaddle = 1 / fromIntegral (paddleStackHeight + 1)

drawBlock :: Float -> Block -> Picture
drawBlock h (B BRed  ) = color red   $ rectangleSolid blockWidth h
drawBlock h (B BBlue ) = color blue  $ rectangleSolid blockWidth h
drawBlock h (B BGreen) = color green $ rectangleSolid blockWidth h

drawMaybeBlock :: Float -> Maybe Block -> Picture
drawMaybeBlock h Nothing  = color white $ rectangleWire blockWidth h
drawMaybeBlock h (Just x) = drawBlock h x 

drawLaneVarHeightLength :: Float -> Int -> Lane -> Picture
drawLaneVarHeightLength h l = Translate 0 (h / 2) . Pictures . zipWith (\y -> Translate 0 (y / fromIntegral l) . drawMaybeBlock h) (map fromIntegral [0..] :: [Float])

drawLanesWith :: (Lane -> Picture) -> [Lane] -> Picture
drawLanesWith f = Translate (blockWidth / 2) 0 . Pictures . zipWith (\x -> Translate (x / fromIntegral lanesCount) 0 . f) (map fromIntegral [0..] :: [Float])

drawLane :: Lane -> Picture
drawLane  =  drawLaneVarHeightLength blockHeightLane laneLength
drawLanes :: [Lane] -> Picture
drawLanes = drawLanesWith drawLane

drawStackingLane :: Lane -> Picture
drawStackingLane = drawLaneVarHeightLength blockHeightStack stackHeight . reverse
drawStackingArea :: [Lane] -> Picture
drawStackingArea = drawLanesWith drawStackingLane

drawPaddleArea :: WorldState -> Picture
drawPaddleArea Running{ paddlePosition = pos, paddleStack = stk } = 
  drawLanesWith 
    (drawLaneVarHeightLength blockHeightPaddle (paddleStackHeight + 1)) $
    (replicate pos []) ++ [Nothing : map Just (reverse stk)] ++ replicate (lanesCount - pos - 1) []