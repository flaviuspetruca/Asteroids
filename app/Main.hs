module Main where

import Lib
import Menu
import Logic
import Graphics.Gloss

main :: IO ()
main = animate (FullScreen) black frame
  where frame :: Float -> Picture
        frame seconds = render $ enterName seconds initialState
--main = displayIO (FullScreen) black (pure (render initialState))
