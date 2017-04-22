module Config where

import Graphics.Gloss.Data.Display (Display (InWindow))
import Graphics.Gloss.Data.Color

screenWidth, screenHeight :: Int
screenWidth = 1366
screenHeight = 720

aspectRatio :: Float
aspectRatio = fromIntegral screenWidth / fromIntegral screenHeight

displayMode :: Display
displayMode = InWindow "HasKlax" (screenWidth, screenHeight) (0, 0)

bgColor :: Color 
bgColor = white

fps :: Int 
fps = 60

-- tick length in seconds
tickLength :: Float
tickLength = 0.15

-- how many ticks for one new block
-- needs to be a multiple of blockSpeed, so blocks can move when new blocks appear
blockEvery :: Int
blockEvery = 5 * blockSpeed

-- how many ticks per block movement
blockSpeed :: Int
blockSpeed = 2

laneLength, lanesCount, stackHeight, klaxMinLength, paddleStackHeight :: Int
laneLength = 20
lanesCount = 5
stackHeight = 5
klaxMinLength = 3
paddleStackHeight = 4
