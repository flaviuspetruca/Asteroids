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

import Struct
import Menu
import Game

-- | Convert a game state into a picture.
render :: GameState -> Picture
render (MkEnterName pics _) = pictures pics
render (MkMainMenu pics _ _ _) = pictures pics
render (MkGameState ks c (MkPlayer _ gs im (x,y) _ l o oo) en b _ hd _ _ _ )
    | l > 0 = pictures (scoreRender : gameRender)
    | otherwise = pictures [deadText, makeText ("Score: " ++ show gs) (-260) (-40)]
      where gameRender  | fst hd    = case () of 
                                      () | c `mod` 5 == 0 -> mkPlayer im black x y o:(enemies++bullets)
                                         | otherwise -> mkPlayer im white x y o:(enemies++bullets)
                        | otherwise = mkPlayer im white x y o:(enemies++bullets)
            scoreRender = makeText (show gs) (-800) 200
            enemies     = map (createPicture white) en
            eb = [b | (Spaceship _ _ _ b)<-en]
            bullets     = map (\(MkBullet _ (newX, newY) o _) -> 
                            translate newX newY $ rotate (360-o) $ color white $ ThickCircle 1.5 3) (b++eb)

render (MkPauseMenu g pics) = pictures pics
render (MkGameOver n gs _)
  = pictures [deadText, nameScore, playAgain, replayBorder, backText, backBorder]
    where nameScore = makeText ("Score: " ++ show gs) (-132)  200
          playAgain = makeText "Play again"           (-140) (-40)
          backText  = makeText "Back to menu"         (-200) (-120)

