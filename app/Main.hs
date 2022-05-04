module Main where

import Graphics.Gloss ( play )
import System.Random ( randomIO, mkStdGen )
import Data.List ((\\))

import Minesweeper ( makeIntList, initialGame )
import Rendering ( backgroundColour, gameAsPicture, window )
import Logic ( transformGame )

--where the magic happens
main :: IO ()
main = do
    --both used as seeds for pseudo-random generators below
    num1 <- randomIO :: IO Int
    num2 <- randomIO :: IO Int

    play window backgroundColour 30 (initialGame (makeIntList [] (mkStdGen num1) (mkStdGen num2))) gameAsPicture transformGame (const id)

