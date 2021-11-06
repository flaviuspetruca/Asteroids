{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module View where
-- VIEW --
-- For rendering the data in pictures to be displayed on the screen.

import Data.List
import Graphics.Gloss
import Data.Number.CReal
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char, SpecialKey, MouseButton),
      KeyState(Up, Down),
      SpecialKey(KeySpace, KeyEnter, KeyBackspace),
      Event(EventKey),
      MouseButton (LeftButton) )
import qualified GHC.Base as S
import System.Random
import GJK.Collision
import GJK.Support
import Graphics.Gloss.Interface.IO.Interact (Key(SpecialKey))
import Graphics.Gloss.Interface.Environment

import Struct
import Menu
import Game

-- | Convert a game state into a picture.
render :: GameState -> Picture
render (MkEnterName pics _) = pictures pics
render (MkMainMenu pics _ _ _) = pictures pics
render (MkPauseMenu g pics) = pictures pics
render (MkGameOver n gs _)
  = pictures [deadText, nameScore, playAgain, replayBorder, backText, backBorder]
    where nameScore = makeText ("Score: " ++ show gs) (-132)  200
          playAgain = makeText "Play again"           (-140) (-40)
          backText  = makeText "Back to menu"         (-200) (-120)

middle :: Float -> Float
middle x = x/2
