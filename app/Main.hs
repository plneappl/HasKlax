module Main where

import Config
import Drawing
import Events
import Lib 
import WorldState
import System.Random
import Graphics.Gloss.Interface.IO.Game (playIO)

main :: IO ()
main = do
  r <- getStdGen
  playIO displayMode bgColor fps (initialWorld r) (return . drawWorldState) eventHandler onTick
