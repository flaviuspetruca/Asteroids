module Logic where

import Data.List
import Graphics.Gloss
import Data.Number.CReal
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import qualified GHC.Base as S
import System.Random
import GJK.Collision
import GJK.Support

import Struct
import Menu
import Window

-- | Convert a game state into a picture.
render :: GameState -> Picture
render (MkEnterName pics _) = pictures pics
render (MkMainMenu pics _ _) = pictures pics
render (MkHighScore pics _ _ _ _) = pictures pics
render (MkGameState ks _ (MkPlayer _ _ (x,y) _ _ o oo) en _ _ _ _ _ )
      = pictures (mkPlayer white x y o:enemies)
      where enemies = map (createPicture white) en
render (MkPauseMenu g pics) = pictures pics

update :: Float -> GameState -> GameState
--update _ = enterName
update seconds (MkEnterName boxes name) = (MkEnterName boxes name)
update seconds (MkMainMenu boxes name score) = (MkMainMenu boxes name score)
update seconds (MkHighScore boxes name score inGame g) = (MkHighScore boxes name score inGame g)
update seconds (MkPauseMenu g boxes) = (MkPauseMenu g boxes)
update sec (MkGameState ks s (MkPlayer n gs (x,y) vel l o oo) e a d st p r)
  | Char 'w' `elem` ks && Char 'a' `elem` ks = if oo /= o && vel > 0
                                                then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.05)) l (newOr o    5) (newOr o   5)) npe a d st p r)
                                                else movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel 0.3) l (newOr o    5) (newOr o   5)) npe a d st p r)
  | Char 'w' `elem` ks && Char 'd' `elem` ks = if oo /= o && vel > 0
                                                then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.05)) l (newOr o (-5)) (newOr o   5)) npe a d st p r)
                                                else movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel 0.3) l (newOr o (-5)) (newOr o   5)) npe a d st p r)
  | Char 'w' `elem` ks = if oo/=o && vel > 0
                          then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) o vel) (acceleration vel (-0.1)) l o o) npe a d st p r)
                          else movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) o vel) (acceleration vel 0.3) l o o) npe a d st p r)
  | Char 'a' `elem` ks = if vel > 0
                            then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.07)) l (newOr o   5) oo) npe a d st p r)
                            else movePlayer sec (MkGameState ks s (MkPlayer n gs (x,y) vel l (newOr o   5) oo) npe a d st p r)
  | Char 'd' `elem` ks = if vel > 0
                            then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.07)) l (newOr o (-5)) oo) npe a d st p r)
                            else movePlayer sec (MkGameState ks s (MkPlayer n gs (x,y) vel l (newOr o (-5)) oo) npe a d st p r)
  | otherwise          = movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.07)) l o oo) npe a d st p r)
  where
        newOr :: Float -> Float -> Float
        newOr o x | (o + x) >= 360 = o + x - 360
                  | otherwise = o + x
        newPos :: (Float, Float) -> Orientation -> Velocity -> (Float, Float)
        newPos (x,y) o v  | v == 0 = (x,y)
                          | otherwise = case () of
                                        ()  | o >= 0  && o <= 90  -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                            | o > 90  && o <= 180 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                            | o > 180 && o <= 270 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                            | otherwise           -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
        newPosEnemies :: [Enemy] -> Position -> [Enemy]
        newPosEnemies es (x1,y1) = map f es
          where f (Asteroid s p or) | distance2 (x1,y1) (newPos p or 1.5) < 37 = Spaceship Small (0,0) 0
                                    | outOfViewBool (newPos p or 1.5) 900 600 = Asteroid s (outOfViewCoord (newPos p or 1.5) 900 600) or
                                    | otherwise = Asteroid s (newPos p or 1.5) or
                f sp = sp
        
        npe = newPosEnemies e (x,y)

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyEnter) _ _ _) en@(MkEnterName boxes currName)
  | length currName > 0 = (MkMainMenu updatedBoxes currName 0)
  | otherwise = (MkEnterName boxes currName)
  where updatedBoxes = makeText currName (-460) (40) : menuBox
handleKeys (EventKey (SpecialKey KeyEnter) _ _ _) (MkHighScore boxes currName score inGame g)
  | inGame = (MkPauseMenu g pauseBox)
  | otherwise = (MkMainMenu updatedMenu currName score)
    where updatedMenu = makeText currName (-460) (40) : menuBox
handleKeys (EventKey (Char '\b') Down _ _) (MkEnterName boxes currName)
  = (MkEnterName updatedBoxes updatedName)
  where updatedName
          | length currName > 0 = take (length currName - 1) currName
          | otherwise = currName
        updatedBoxes = makeText updatedName (namePos) (-40) : enterBox
        namePos = (-60) - fromIntegral (length (currName) * 15)
handleKeys (EventKey (Char ch) Down _ _) (MkEnterName boxes currName)
  = (MkEnterName updatedBoxes updatedName)
  where updatedName
          | length currName < 12 && ch /= '\b' = currName ++ [ch]
          | otherwise = currName
        updatedBoxes = makeText updatedName (namePos) (-40) : enterBox
        namePos = (-60) - fromIntegral (length (currName) * 15)
handleKeys (EventKey (MouseButton LeftButton) Down _ mousePos) mm@(MkMainMenu boxes currName score)
  | x > (-460) && x < 500 && y > (-60) && y < 20 = game
  | x > (-460) && x < 500 && y > (-142) && y < (-62) = (MkHighScore currScore currName score False mm)
  | otherwise = mm -- REMOVE?
    where updatedBoxes = makeText currName (-460) (80) : menuBox
          (x, y) = mousePos
          currScore = [(makeScore currName score), (makeText "Press Enter to go back" (-240) (-80))]
          updatedPause = makeText currName (-460) (40) : pauseBox
          game = (MkGameState [] 0 player enemies allArtillery Easy True False sg)
          player = (MkPlayer currName 0 (0,0) 0.0 3 90 90)
          enemies = fst $ mkAsteroids 10 (mkStdGen 2)
          allArtillery = [(1,1,1), (1,2,2)]
          sg = snd $ mkAsteroids 10 (mkStdGen 2)
--handleKeys (EventKey (MouseButton LeftButton) Down _ mousePos) (MkGameState box name score hp True pos)
--  | x > (-460) && x < 500 && y > (-224) && y < (-144) = (MainMenu box name score)
--  | otherwise = (MkGameState box name score hp True pos)
--    where (x, y) = mousePos
handleKeys (EventKey key@(Char c) state _ _) g@(MkGameState ks s (MkPlayer n gs (x,y) vel l o oo) e a d st p r)
  | c == 'w' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
  | c == 'a' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
  | c == 'd' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
handleKeys (EventKey (Char 'p') Down _ _) g@(MkGameState ks s (MkPlayer n gs (x,y) 0.0 l o oo) e a d st p r)
  = (MkPauseMenu g pauseBox)
handleKeys (EventKey (Char 'p') Down _ _) (MkPauseMenu g boxes)
  = g
handleKeys (EventKey (MouseButton LeftButton) Down _ mousePos) pm@(MkPauseMenu game@(MkGameState ks s player@(MkPlayer n gs (x,y) vel l o oo) e a d st p r) boxes)
  | x' > (-460) && x' < 500 && y' > (-60) && y' < 20 = game
  | x' > (-460) && x' < 500 && y' > (-142) && y' < (-62) = (MkHighScore currScore n s True game) -- TBD
  | x' > (-460) && x' < 500 && y' > (-224) && y' < (-144) = (MkMainMenu updatedBoxes n gs)
  | otherwise = pm
    where (x', y') = mousePos
          currScore = [(makeScore n gs), (makeText "Press Enter to go back" (-240) (-80))]
          updatedBoxes = makeText n (-460) (40) : menuBox

handleKeys _ game = game

