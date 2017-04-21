module Drawing where

import Graphics.Gloss

import Config
import Events
import WorldState
import BlockDrawing
import Data.Functor.Identity (Identity)

upscaleScreen, upscalePlayingField, placePlayingField :: Picture -> Picture
upscaleScreen = Scale (fromIntegral screenWidth) (fromIntegral screenHeight)
upscalePlayingField = let 
  smin = fromIntegral $ min screenWidth screenHeight
  width = smin
  height = 2 * smin in
  Scale width height

placePlayingField p = Pictures [color black $ rectangleSolid 1 1, Scale sx sy p] where
  fillX = aspectRatio >= 0.5
  sx = if fillX then 1 / (aspectRatio * 2) else 1
  sy = if fillX then 1 else aspectRatio * 2

composePlayingField :: Picture -> Picture -> Picture -> Picture
composePlayingField incoming paddleRegion dropoff = Pictures [
    Translate 0   0.2  $ Scale 1 0.6  incoming
  , Translate 0 (-0.2) $ Scale 1 0.2 paddleRegion
  , Translate 0 (-0.4) $ Scale 1 0.2 dropoff]

c01toc11 :: Picture -> Picture
c01toc11 = Translate (-0.5) (-0.5)


drawWorldState :: WorldState -> Picture
drawWorldState ws@Running {lanes = l, stackingArea = sa} = 
  upscaleScreen $ placePlayingField $ composePlayingField 
    (c01toc11 $ drawLanes l) 
    (c01toc11 $ drawPaddleArea ws) 
    (c01toc11 $ drawStackingArea sa)