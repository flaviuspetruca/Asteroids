module Logic where

import Graphics.Gloss

data GameState -- TBD: scores :: [Int]
  = EnterName
    { textBoxes :: [Picture],
      name :: String }
  | MainMenu
    { textBoxes :: [Picture],
      name :: String,
      score :: Int }
  | HighScore
    { textBoxes :: [Picture],
      name :: String,
      score :: Int }
  | GameState
    { textBoxes :: [Picture],
      name :: String,
      score :: Int,
      lives :: Float,
      paused :: Bool,
      position :: (Float, Float) } deriving Show
