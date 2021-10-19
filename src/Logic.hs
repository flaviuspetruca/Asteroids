module Logic where

import Menu
import Graphics.Gloss

data GameState'
  = State
    { position :: (Float, Float),
      lives :: Float,
      paused :: Bool }
  | MainMenu
    { textBoxes :: [Picture] } deriving Show

initialState :: GameState'
initialState = MainMenu name

render :: GameState' -> Picture
render (MainMenu pics)
  = pictures pics

enterName :: Float -> GameState' -> GameState'
enterName seconds game
  | seconds > 2.0 = game { textBoxes = menu }
  | otherwise = game
