{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Logic where

import Data.List
import Graphics.Gloss
import Data.Number.CReal
import Graphics.Gloss.Data.ViewPort
--import Graphics.Gloss.Interface.Pure.Game
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
import Window

{- spaceship :: Picture 
spaceship = pictures [Line[(10*ss, 0), (5*ss,-3*ss),(-5*ss,-3*ss),(-10*ss,0),(10*ss,0)],
                      Line [(-10*ss, 0), (-5*ss,3*ss),(5*ss,3*ss),(10*ss,0),(-10*ss,0)],
                      Line [(-3.5*ss, 6*ss), (-5*ss,3*ss),(5*ss,3*ss),(3.5*ss,6*ss),(-3.5*ss,6*ss)]] -}

-- | Convert a game state into a picture.
render :: GameState -> Picture
render (MkEnterName pics _) = pictures pics
render (MkMainMenu pics _ _) = pictures pics
render (MkHighScore pics _ _ _ _) = pictures pics
render (MkGameState ks _ (MkPlayer _ _ im (x,y) _ l o oo) en b _ _ _ _ )
    | l > 0 = pictures (mkPlayer im white x y o:(enemies++bullets))
    | otherwise = pictures [ color white $ Circle 20]
    where enemies = map (createPicture white) en
          bullets = map (\(MkBullet op (newX, newY) o _) -> translate newX newY $ rotate (360-o) $ color white $ ThickCircle 1.5 3) b
render (MkPauseMenu g pics) = pictures pics

update :: Float -> GameState -> GameState
update seconds (MkEnterName boxes name) = (MkEnterName boxes name)
update seconds (MkMainMenu boxes name score) = (MkMainMenu boxes name score)
update seconds (MkHighScore boxes name score inGame g) = (MkHighScore boxes name score inGame g)
update seconds (MkPauseMenu g boxes) = (MkPauseMenu g boxes)
update sec g@(MkGameState ks s (MkPlayer n gs im (x,y) vel l o oo) e a d st p r)
  | SpecialKey KeySpace `elem` ks && Char 'w' `elem` ks && Char 'd' `elem` ks
  = case  () of
          () | oo/=o && vel > 0 -> movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel (-0.05)) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb bullets e gs) d st p r)
             | otherwise        -> movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel 0.3) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb bullets e gs) d st p r)
  | SpecialKey KeySpace `elem` ks && Char 'w' `elem` ks && Char 'a' `elem` ks
  = case  () of
          () | oo/=o && vel > 0 -> movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel (-0.05)) updatedLives (newOr o 3) (newOr o  (-3))) npe (npb bullets e gs) d st p r)
             | otherwise        -> movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel 0.3) updatedLives (newOr o 3) (newOr o (-3))) npe (npb bullets e gs) d st p r)

  | Char 'w' `elem` ks && Char 'd' `elem` ks
    = if oo /= o && vel > 0
        then movePlayer sec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel (-0.05)) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb a e gs) d st p r)
        else movePlayer sec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel 0.3) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb a e gs) d st p r)
  | Char 'w' `elem` ks && Char 'a' `elem` ks
    = if oo /= o && vel > 0
      then movePlayer sec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel (-0.05)) updatedLives (newOr o    3) (newOr o   (-3))) npe (npb a e gs) d st p r)
      else movePlayer sec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel 0.3) updatedLives (newOr o    3) (newOr o   (-3))) npe (npb a e gs) d st p r)

  | SpecialKey KeySpace `elem` ks && Char 'w' `elem` ks
    = case  () of
            () | oo/=o && vel > 0 -> movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos o) (acceleration vel (-0.1)) updatedLives o o) npe (npb bullets e gs) d st p r)
               | otherwise        -> movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos o) (acceleration vel 0.3) updatedLives o o) npe (npb bullets e gs) d st p r)

  | SpecialKey KeySpace `elem` ks && Char 'a' `elem` ks
    = movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives (newOr o 3) oo) npe (npb bullets e gs) d st p r)
  | SpecialKey KeySpace `elem` ks && Char 'd' `elem` ks
    = movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives (newOr o (-3)) oo) npe (npb bullets e gs) d st p r)
  | SpecialKey KeySpace `elem` ks
    = movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives o oo) npe (npb bullets e gs) d st p r)

  | Char 'w' `elem` ks
    = if oo/=o && vel > 0
        then movePlayer sec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos o) (acceleration vel (-0.1)) updatedLives o o) npe (npb a e gs) d st p r)
        else movePlayer sec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos o) (acceleration vel 03) updatedLives o o) npe (npb a e gs) d st p r)
  | Char 'a' `elem` ks
    = if vel > 0
        then movePlayer sec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives (newOr o   3) oo) npe (npb a e gs) d st p r)
        else movePlayer sec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) vel updatedLives (newOr o   5) oo) npe (npb a e gs) d st p r)
  | Char 'd' `elem` ks
    = if vel > 0
        then movePlayer sec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives (newOr o (-3)) oo) npe (npb a e gs) d st p r)
        else movePlayer sec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) vel updatedLives (newOr o (-3)) oo) npe (npb a e gs) d st p r)
  | otherwise = movePlayer sec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives o oo) npe (npb a e gs) d st p r)
  where
      newOr :: Float -> Float -> Float
      newOr o x | (o + x) >= 360 = o + x - 360
                | otherwise = o + x

      changePlayerPos :: Orientation-> Position
      changePlayerPos or | updatedLives /= l   = (0,0)
                         | otherwise = newPos (x,y) o vel

      bullets | length a < 5 = MkBullet (x,y) (x,y) o vel : a
              | otherwise    = a

      newPosEnemies :: [Enemy] -> Position -> [Bullet] -> Int -> ([Enemy],Int)
      newPosEnemies [] _ _ lives = ([], lives)
      newPosEnemies (e@Spaceship{}:es) _ _ lives= ([], lives)
      newPosEnemies (e@(Asteroid s p or):es) (x1,y1) bs lives
        | notCollisionEB e bs == False
          = case  () of
                  ()  | s /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives), newLives)
                      | otherwise  -> newPosEnemies es (x1,y1) bs lives
        | distance2 (x1,y1) (newPos p or 1.5) > ds  = (f e : fst (newPosEnemies es (x1,y1) bs lives), newLives)
        | otherwise
          = case  () of
                  () | s /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives), newLives - 1)
                    | otherwise  -> (fst $ newPosEnemies es (x1,y1) bs lives, newLives-1)
        where f (Asteroid s p or) | outOfViewBool (newPos p or enemVel) 1000 700 = Asteroid s (outOfViewCoord (newPos p or enemVel) 1000 700) or
                                  | otherwise = Asteroid s (newPos p or enemVel) or
              f sp = sp
              ds  | s == Large  = 40
                  | s == Med    = 35
                  | otherwise   = 17

              enemVel | s == Large  = 1.5
                      | s == Med    = 2
                      | otherwise   = 2.5

              newLives = snd (newPosEnemies es (x1,y1) bs lives)

      newPosBullets :: [Bullet] -> [Enemy] -> GameScore -> ([Bullet],GameScore)
      newPosBullets [] _ gs = ([], gs)
      newPosBullets (b@(MkBullet op np orB vB):bs) es gs
        | fst (notCollisionBE b es) == False = newPosBullets bs es (newGs (snd (notCollisionBE b es)))
        | distance2 op np > 400 = newPosBullets bs es gs
        | otherwise = (MkBullet op (newPos np orB (vB+5)) orB vB : fst(newPosBullets bs es gs), snd (newPosBullets bs es gs))

      notCollisionBE :: Bullet -> [Enemy] -> (Bool,Size)
      notCollisionBE _ [] = (True, Small)
      notCollisionBE bull@(MkBullet op np orB _) (e@(Asteroid s p orA):es)  | distance2 p np < dBE s = (False,s)
                                                                            | otherwise = (True && fst (notCollisionBE bull es), snd $ notCollisionBE bull es)
      notCollisionBE _ _ = (True, Small)

      notCollisionEB :: Enemy -> [Bullet] -> Bool
      notCollisionEB _ [] = True
      notCollisionEB enem@(Asteroid s p orA) (b@(MkBullet op np orB _):bs)  | distance2 p np < dBE s = False
                                                                            | otherwise = True && notCollisionEB enem bs
      notCollisionEB _ _ = True

      dBE :: Size -> Float
      dBE s | s == Large = 40
            | s == Med = 20
            | otherwise = 15

      newGs astSize | astSize == Large = gs + 20
                    | astSize == Med   = gs + 50
                    | otherwise  = gs + 100

      updatedGs = snd $ newPosBullets a e gs
      npe = fst $ newPosEnemies e (x,y) a l
      fire  | floor sec == round sec = True
            | otherwise = False
      newKeys = delete (SpecialKey KeySpace) ks
      updatedLives = snd $ newPosEnemies e (x,y) a l
      npb art ene gs = fst $ newPosBullets art ene gs

newPos :: (Float, Float) -> Orientation -> Velocity -> (Float, Float)
newPos (x,y) o v  | v == 0 = (x,y)
                  | otherwise = case () of
                                ()  | o >= 0  && o <= 90  -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | o > 90  && o <= 180 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | o > 180 && o <= 270 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | otherwise           -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)

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
  | x > (-460) && x < 500 && y > (-224) && y < (-144) = (MkQuitGame)
  | otherwise = mm -- REMOVE?
    where updatedBoxes = makeText currName (-460) (80) : menuBox
          (x, y) = mousePos
          currScore = [(makeScore currName score), (makeText "Press Enter to go back" (-240) (-80))]
          updatedPause = makeText currName (-460) (40) : pauseBox
          game = (MkGameState [] 0 player enemies [] Easy True False sg)
          player = (MkPlayer currName  0 False (0,0) 0.0 3 90 90)
          enemies = fst $ mkAsteroids 5 (mkStdGen 2)
          sg = snd $ mkAsteroids 5 (mkStdGen 2)

handleKeys (EventKey key@(Char c) state _ _) g@(MkGameState ks s (MkPlayer n gs im (x,y) vel l o oo) e a d st p r)
  | c == 'w' =  case state of Down  -> g { keys = insert key ks}
                              Up    -> g { keys = delete key ks}
  | c == 'a' =  case state of Down  -> g { keys = insert key ks}
                              Up    -> g { keys = delete key ks}
  | c == 'd' =  case state of Down  -> g { keys = insert key ks}
                              Up    -> g { keys = delete key ks}
handleKeys (EventKey key@(SpecialKey KeySpace) state _ _)  g@(MkGameState ks s (MkPlayer n gs im (x,y) v l o oo) e a d st p r)
        | state == Down = case () of
                          () | nrPresses >= 5 && nrPresses <= 10 -> g { keys = replicate 5 key ++ filter (\x-> x /= (Char 'n')) ks}
                             | otherwise -> g { keys = insert (Char 'n') ks}
        | otherwise     = case () of
                          () | nrPresses >= 1 && nrPresses <= 5 -> g { keys = key: filter (\x-> x /= (Char 'n')) ks}
                             | otherwise -> g
        where
          nrPresses = length (filter (\x-> x ==  (Char 'n')) ks)
handleKeys (EventKey (Char 'p') Down _ _) g@(MkGameState ks s (MkPlayer n gs im (x,y) 0.0 l o oo) e a d st p r)
  = (MkPauseMenu g pauseBox)
handleKeys (EventKey (Char 'p') Down _ _) (MkPauseMenu g boxes)
  = g
handleKeys (EventKey (MouseButton LeftButton) Down _ mousePos) pm@(MkPauseMenu game@(MkGameState ks s player@(MkPlayer n gs im (x,y) vel l o oo) e a d st p r) boxes)
  | x' > (-460) && x' < 500 && y' > (-60) && y' < 20 = game
  | x' > (-460) && x' < 500 && y' > (-142) && y' < (-62) = (MkHighScore currScore n s True game) -- TBD
  | x' > (-460) && x' < 500 && y' > (-224) && y' < (-144) = (MkMainMenu updatedBoxes n gs)
  | otherwise = pm
    where (x', y') = mousePos
          currScore = [(makeScore n gs), (makeText "Press Enter to go back" (-240) (-80))]
          updatedBoxes = makeText n (-460) (40) : menuBox
handleKeys _ game = game

