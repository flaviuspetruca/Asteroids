{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Logic where
-- LOGIC --
-- For updating the game each step,
-- as well as handling input keys.

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
import GHC.IO.Buffer (checkBuffer)

import Struct
import Menu
import Game

-- Updates the GameState one step further
update :: Float -> GameState -> GameState
update seconds (MkEnterName boxes name) = (MkEnterName boxes name)
update seconds (MkMainMenu boxes name score _) = (MkMainMenu boxes name score False)
update seconds (MkHighScore boxes name score inGame g) = (MkHighScore boxes name score inGame g)
update seconds (MkPauseMenu g boxes) = (MkPauseMenu g boxes)
update sec g@(MkGameOver _ _ _) = g
update sec g@(MkGameState _ _ (MkPlayer _ _ _ (x,y) _ _ _ _) [] _ _ _ _ _ r) = g {enemies= fst $ mkAsteroids 5 (x,y) r}
update sec g@(MkGameState ks c (MkPlayer n gs im (x,y) vel l o oo) enemies a d hd st p r)
  | l == 0 = (MkGameOver n gs 0)
  | SpecialKey KeySpace `elem` ks && Char 'w' `elem` ks && Char 'd' `elem` ks
  = case  () of
          () | oo/=o && vel > 0 -> movePlayer sec (MkGameState newKeys (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos oo)) (acc vel (-0.05)) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb bullets) d (snd(cgPlPos oo)) st p r)
             | otherwise        -> movePlayer sec (MkGameState newKeys (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos oo)) (acc vel 0.3) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb bullets) d (snd(cgPlPos oo)) st p r)
  | SpecialKey KeySpace `elem` ks && Char 'w' `elem` ks && Char 'a' `elem` ks
  = case  () of
          () | oo/=o && vel > 0 -> movePlayer sec (MkGameState newKeys (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos oo)) (acc vel (-0.05)) updatedLives (newOr o 3) (newOr o  (-3))) npe (npb bullets) d (snd(cgPlPos oo)) st p r)
             | otherwise        -> movePlayer sec (MkGameState newKeys (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos oo)) (acc vel 0.3) updatedLives (newOr o 3) (newOr o (-3))) npe (npb bullets) d (snd(cgPlPos oo)) st p r)

  | Char 'w' `elem` ks && Char 'd' `elem` ks
    = if oo /= o && vel > 0
        then movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos oo)) (acc vel (-0.05)) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb a) d (snd(cgPlPos oo)) st p r)
        else movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos oo)) (acc vel 0.3) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb a) d (snd(cgPlPos oo)) st p r)
  | Char 'w' `elem` ks && Char 'a' `elem` ks
    = if oo /= o && vel > 0
      then movePlayer sec (MkGameState ks (c+1)(MkPlayer n updatedGs fire (fst(cgPlPos oo)) (acc vel (-0.05)) updatedLives (newOr o    3) (newOr o   (-3))) npe (npb a) d (snd(cgPlPos oo)) st p r)
      else movePlayer sec (MkGameState ks (c+1)(MkPlayer n updatedGs fire (fst(cgPlPos oo)) (acc vel 0.3) updatedLives (newOr o    3) (newOr o   (-3))) npe (npb a) d (snd(cgPlPos oo)) st p r)

  | SpecialKey KeySpace `elem` ks && Char 'w' `elem` ks
    = case  () of
            () | oo/=o && vel > 0 -> movePlayer sec (MkGameState newKeys (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos o)) (acc vel (-0.1)) updatedLives o o) npe (npb bullets) d (snd(cgPlPos o)) st p r)
               | otherwise        -> movePlayer sec (MkGameState newKeys (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos o)) (acc vel 0.3) updatedLives o o) npe (npb bullets) d (snd(cgPlPos o)) st p r)

  | SpecialKey KeySpace `elem` ks && Char 'a' `elem` ks
    = movePlayer sec (MkGameState newKeys (c+1) (MkPlayer n updatedGs False (fst(cgPlPos oo)) (acc vel (-0.07)) updatedLives (newOr o 3) oo) npe (npb bullets) d (snd(cgPlPos oo)) st p r)
  | SpecialKey KeySpace `elem` ks && Char 'd' `elem` ks
    = movePlayer sec (MkGameState newKeys (c+1) (MkPlayer n updatedGs False (fst(cgPlPos oo)) (acc vel (-0.07)) updatedLives (newOr o (-3)) oo) npe (npb bullets) d (snd(cgPlPos oo)) st p r)
  | SpecialKey KeySpace `elem` ks
    = movePlayer sec (MkGameState newKeys (c+1) (MkPlayer n updatedGs False (fst(cgPlPos oo)) (acc vel (-0.07)) updatedLives o oo) npe (npb bullets) d (snd(cgPlPos oo)) st p r)

  | Char 'w' `elem` ks
    = if oo/=o && vel > 0
        then movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos o)) (acc vel (-0.1)) updatedLives o o) npe (npb a) d (snd(cgPlPos o)) st p r)
        else movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs fire (fst(cgPlPos o)) (acc vel 03) updatedLives o o) npe (npb a) d (snd(cgPlPos o)) st p r)
  | Char 'a' `elem` ks
    = if vel > 0
        then movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs False (fst(cgPlPos oo)) (acc vel (-0.07)) updatedLives (newOr o   3) oo) npe (npb a) d (snd(cgPlPos oo)) st p r)
        else movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs False (fst(cgPlPos oo)) vel updatedLives (newOr o   5) oo) npe (npb a) d (snd(cgPlPos oo)) st p r)
  | Char 'd' `elem` ks
    = if vel > 0
        then movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs False (fst(cgPlPos oo)) (acc vel (-0.07)) updatedLives (newOr o (-3)) oo) npe (npb a) d (snd(cgPlPos oo)) st p r)
        else movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs False (fst(cgPlPos oo)) vel updatedLives (newOr o (-3)) oo) npe (npb a) d (snd(cgPlPos oo)) st p r)
  | otherwise = movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs False (fst(cgPlPos oo)) (acc vel (-0.07)) updatedLives o oo) npe (npb a) d (snd(cgPlPos oo)) st p r)
  where -- MOST OF YOUR GAME LOGIC FUNCTIONS ARE NOW IN GAME.HS (THERE MAY BE BUGS BECAUSE OF THIS)
      newOr :: Float -> Float -> Float
      newOr o x | (o + x) >= 360 = o + x - 360
                | otherwise = o + x

      cgPlPos :: Orientation -> (Position, Bool)
      cgPlPos or | updatedLives /= l   = ((0,0), True)
                 | otherwise = (newPos (x,y) o vel, False)

      bullets | length a < 5 = MkBullet (x,y) (x,y) o vel : a
              | otherwise    = a

      fst3 (x,_,_) = x
      snd3 (_,y,_) = y
      trd3 (_,_,z) = z

      dBE :: Size -> Float
      dBE s | s == Large = 47
            | s == Med = 28
            | otherwise = 14

      updatedGs = snd $ newPosBullets a enemies gs

      npe | c `mod` 1000 == 0 = fst $ newPosEnemies enemies (fst (mkEnemy rS 's' (x,y) r):enemies) (x,y) a l c r o (x,y)
          | otherwise         = fst(newPosEnemies enemies (getSpaceships enemies) (x,y) a l c r o (x,y)) ++ fst(newPosEnemies enemies (fst (newPosEnemies enemies (getAsteroids enemies) (x,y) a l c r o (x,y))) (x,y) enemyBullets l c r o (x,y))
            where enemyBullets  = [b | (Spaceship _ _ _ b)<-enemies]


      rS = [Large, Med]!!fst (randomR (0,1) r)

      fire  | c `mod` 5 == 0 = True
            | otherwise = False

      newKeys = delete (SpecialKey KeySpace) ks
      updatedLives = snd $ newPosEnemies enemies enemies (x,y) a l c r o (x,y)
      npb art = fst $ newPosBullets art enemies gs

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyEnter) _ _ _) en@(MkEnterName boxes currName)
  | length currName > 0 = (MkMainMenu updatedBoxes currName 0 False)
  | otherwise = (MkEnterName boxes currName)
  where updatedBoxes = makeText (show 0) (460) (40) : makeText currName (-460) (40) : menuBox

handleKeys (EventKey (SpecialKey KeyEnter) _ _ _) (MkHighScore boxes currName score inGame g)
  | inGame = (MkPauseMenu g pauseBox)
  | otherwise = (MkMainMenu updatedMenu currName score False)
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
          | length currName < 12 && ch /= '\b' && ch /= ':' = currName ++ [ch]
          | otherwise = currName
        updatedBoxes = makeText updatedName (namePos) (-40) : enterBox
        namePos = (-60) - fromIntegral (length (currName) * 15)

handleKeys (EventKey (MouseButton LeftButton) Down _ mousePos) mm@(MkMainMenu boxes currName score _)
  | x > (-460) && x < 500 && y > (-60) && y < 20 = game
  | x > (-460) && x < 500 && y > (-142) && y < (-62) = (MkHighScore currScore currName score False mm)
  | x > (-460) && x < 500 && y > (-224) && y < (-144) = (MkQuitGame)
  | otherwise = mm -- REMOVE?
    where updatedBoxes = makeText currName (-460) (80) : menuBox
          (x, y) = mousePos
          currScore = [(makeScore currName score), (makeText "Press Enter to go back" (-240) (-80))]
          updatedPause = makeText currName (-460) (40) : pauseBox
          game = (MkGameState [] 1 player enemies [] Easy False True False sg)
          player = (MkPlayer currName 0 False (0,0) 0.0 3 90 90)
          enemies = fst (mkAsteroids 5 (0,0) (mkStdGen 2))
          sg = snd $ mkAsteroids 5 (0,0) (mkStdGen 2)

handleKeys (EventKey key@(Char c) state _ _) g@(MkGameState ks cnt (MkPlayer n gs im (x,y) vel l o oo) e a d hd st p r)
  | c == 'w' =  case state of Down  -> g { keys = insert key ks}
                              Up    -> g { keys = delete key ks}
  | c == 'a' =  case state of Down  -> g { keys = insert key ks}
                              Up    -> g { keys = delete key ks}
  | c == 'd' =  case state of Down  -> g { keys = insert key ks}
                              Up    -> g { keys = delete key ks}
handleKeys (EventKey key@(SpecialKey KeySpace) state _ _)  g@(MkGameState ks cnt(MkPlayer n gs im (x,y) v l o oo) e a d hd st p r)
        | state == Down = case () of
                          () | nrPresses >= 5 && nrPresses <= 10 -> g { keys = replicate 5 key ++ filter (\x-> x /= (Char 'n')) ks}
                             | otherwise -> g { keys = insert (Char 'n') ks}
        | otherwise     = case () of
                          () | nrPresses >= 1 && nrPresses <= 5 -> g { keys = key: filter (\x-> x /= (Char 'n')) ks}
                             | otherwise -> g
        where
          nrPresses = length (filter (\x-> x ==  (Char 'n')) ks)
handleKeys (EventKey (Char 'p') Down _ _) g@(MkGameState ks cnt(MkPlayer n gs im (x,y) 0.0 l o oo) e a d hd st p r)
  = (MkPauseMenu g pauseBox)
handleKeys (EventKey (Char 'p') Down _ _) (MkPauseMenu g boxes)
  = g
handleKeys (EventKey (MouseButton LeftButton) Down _ mousePos) pm@(MkPauseMenu game@(MkGameState ks cnt player@(MkPlayer n gs im (x,y) vel l o oo) e a d hd st p r) boxes)
  | x' > (-460) && x' < 500 && y' > (-60) && y' < 20 = game
  | x' > (-460) && x' < 500 && y' > (-142) && y' < (-62) = (MkHighScore currScore n gs True game) -- TBD
  | x' > (-460) && x' < 500 && y' > (-224) && y' < (-144) = (MkMainMenu updatedBoxes n 0 False)
  | otherwise = pm
    where (x', y') = mousePos
          currScore = [(makeScore n gs), (makeText "Press Enter to go back" (-240) (-80))]
          updatedBoxes = makeText (show 0) (460) (40) : makeText n (-460) (40) : menuBox
handleKeys (EventKey (MouseButton LeftButton) Down _ mousePos) (MkGameOver n gs c)
  | x > (-460) && x < 500 && y > (-60) && y < 20 = newGame
  | x > (-460) && x < 500 && y > (-142) && y < (-62) = (MkMainMenu boxes n gs True)
  where (x, y) = mousePos
        boxes = makeText n (-460) (80) : menuBox
        newGame = (MkGameState [] 1 player enemies [] Easy False True False sg)
        player = (MkPlayer n 0 False (0,0) 0.0 3 90 90)
        enemies = fst (mkAsteroids 5 (0,0) (mkStdGen 2))
        sg = snd $ mkAsteroids 5 (0,0) (mkStdGen 2)
handleKeys _ game = game
