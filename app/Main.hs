module Main where

import Lib
import Struct
import Menu
import Logic
import Window
import Graphics.Gloss
import System.Random

main = play window background fps initialMenuState render handleKeys update


initialMenuState = MkEnterName enterBox ""
