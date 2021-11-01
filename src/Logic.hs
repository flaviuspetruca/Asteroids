{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Logic where

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
import Window
    ( mkPlayer,
      mkEnemy,
      mkAsteroids,
      createPicture,
      outOfViewBool,
      outOfViewCoord,
      movePlayer,
      createSmallerAsteroids,
      distance2,
      acc )
import GHC.IO.Buffer (checkBuffer)

-- | Convert a game state into a picture.
render :: GameState -> Picture
render (MkEnterName pics _) = pictures pics
render (MkMainMenu pics _ _ _) = pictures pics
render (MkGameState ks c (MkPlayer _ gs im (x,y) _ l o oo) en b _ hd _ _ _ )
    | l > 0 = pictures (scoreRender : gameRender)
    | otherwise = pictures [deadText, makeText ("Score: " ++ (show gs)) (-260) (-40)]
      where gameRender | hd == False = (mkPlayer im white x y o:(enemies++bullets))
                       | otherwise   = (mkPlayer im white x y o:(enemies++bullets))
            scoreRender = makeText (show gs) (-800) (200)
            enemies = map (createPicture white) en
            bullets = map (\(MkBullet op (newX, newY) o _) -> translate newX newY $ rotate (360-o) $ color white $ ThickCircle 1.5 3) (b++eb)
            eb = [b | (Spaceship _ _ _ b)<-en]
render (MkPauseMenu g pics) = pictures pics

update :: Float -> GameState -> GameState
update seconds (MkEnterName boxes name) = (MkEnterName boxes name)
update seconds (MkMainMenu boxes name score _) = (MkMainMenu boxes name score False)
update seconds (MkHighScore boxes name score inGame g) = (MkHighScore boxes name score inGame g)
update seconds (MkPauseMenu g boxes) = (MkPauseMenu g boxes)
update sec g@(MkGameState _ _ (MkPlayer _ _ _ (x,y) _ _ _ _) [] _ _ _ _ _ r) = g {enemies= fst $ mkAsteroids 5 (x,y) r}
update sec g@(MkGameState ks c (MkPlayer n gs im (x,y) vel l o oo) enemies a d hd st p r)
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
  | l == 0 = (MkMainMenu menuBox n gs True)
  | otherwise = movePlayer sec (MkGameState ks (c+1) (MkPlayer n updatedGs False (fst(cgPlPos oo)) (acc vel (-0.07)) updatedLives o oo) npe (npb a) d (snd(cgPlPos oo)) st p r)
  where
      newOr :: Float -> Float -> Float
      newOr o x | (o + x) >= 360 = o + x - 360
                | otherwise = o + x

      cgPlPos :: Orientation -> (Position, Bool)
      cgPlPos or | updatedLives /= l   = ((0,0), True)
                 | otherwise = (newPos (x,y) o vel, False)

      bullets | length a < 5 = MkBullet (x,y) (x,y) o vel : a
              | otherwise    = a

      newPosEnemies :: [Enemy] -> Position -> [Bullet] -> Int -> Int -> ([Enemy],Int)
      newPosEnemies [] _ _ lives _ = ([], lives)
      newPosEnemies (e@(Spaceship s p or b):es) (x1,y1) bs lives c
        | notCollisionEB e bs == False || notCollisionSsAs e (getAsteroids enemies) == False = newPosEnemies es (x1, y1) bs lives c
        | distance2 (x1,y1) (newPos p or 1.5) > dss = (f e : fst (newPosEnemies es (x1,y1) bs lives c), newLives)
        | otherwise = (fst $ newPosEnemies es (x1,y1) bs lives c, newLives-1)
        where
              dss | s == Large  = 40
                  | s == Med    = 25
                  | otherwise   = 10

              f (Spaceship s p or b)
                | outOfViewBool (newPos p or enemVel) 1000 700 = Spaceship s (outOfViewCoord (newPos p or enemVel) 1000 700) or (newBulletPos b)
                | otherwise = case  () of
                                    () | c `mod` 300 == 0  -> Spaceship s (newPos p (newOr or rO) enemVel) (newOr or rO) (newBulletPos b)
                                       | otherwise         -> Spaceship s (newPos p or enemVel) or (newBulletPos b)
                where
                  newBulletPos (MkBullet op np ori velo)
                      | fst3 (notCollisionBE (MkBullet op np ori velo) filteredEnemies) == False = MkBullet p p nO velo
                      | distance2 op np > 400 = MkBullet p p nO velo
                      | distance2 np (x,y) < dss = MkBullet p p nO velo
                      | otherwise = MkBullet op (newPos np ori (velo+8)) ori velo

                  nO  | x > 0 && y > 0 = acos ((distance2 p (x,snd p))/(distance2 p (x,y)))*180/pi
                      | x < 0 && y > 0 = 180 - (acos ((distance2 p (x,snd p))/(distance2 p (x,y)))*180/pi) 
                      | x < 0 && y < 0 = 180 + (acos ((distance2 p (x,snd p))/(distance2 p (x,y)))*180/pi)
                      | otherwise      = 270 + (acos ((distance2 p (x,snd p))/(distance2 p (x,y)))*180/pi) 
                  
                  filteredEnemies = [z | z@(Asteroid {})<-enemies]

              rO = list!!fst (randomR (0,15) r)
              list = [-315, -270, -225, -180, -135, -90, -45, 0 ,45, 90, 135, 180, 225, 270, 315]

              enemVel | s == Large  = 2.5
                      | s == Med    = 4
                      | otherwise   = 3.5

              newLives = snd (newPosEnemies es (x1,y1) bs lives c)
              

      newPosEnemies (e@(Asteroid  s p or):es) (x1,y1) bs lives c
        | notCollisionEB e bs == False || notCollisionAsSs e (getSpaceships enemies) == False
          = case  () of
                  ()  | s /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives c), newLives)
                      | otherwise  -> newPosEnemies es (x1,y1) bs lives c
        | distance2 (x1,y1) (newPos p or 1.5) > ds  = (f e : fst (newPosEnemies es (x1,y1) bs lives c), newLives)
        | otherwise
          = case  () of
                  () | s /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives c), newLives - 1)
                     | otherwise  -> (fst $ newPosEnemies es (x1,y1) bs lives c, newLives-1)
        where
          f (Asteroid s p or) | outOfViewBool (newPos p or enemVel) 1000 700 = Asteroid s (outOfViewCoord (newPos p or (enemVel/2)) 1000 700) or
                              | otherwise = Asteroid s (newPos p or (enemVel/2)) or

          ds  | s == Large  = 56
              | s == Med    = 37
              | otherwise   = 20


          enemVel | s == Large  = 1.5
                  | s == Med    = 2
                  | otherwise   = 2.5

          newLives | playerHit enemyBullets = l - 1
                   | otherwise = snd (newPosEnemies es (x1,y1) bs lives c)
              where enemyBullets = [b | (Spaceship _ _ _ b)<-enemies]

          playerHit :: [Bullet] -> Bool
          playerHit [] = False
          playerHit ((MkBullet _ p _ _):bs) | distance2 p (x,y) < dss = True
                                            | otherwise = playerHit bs
            where 
              dss | s == Large  = 40
                  | s == Med    = 25
                  | otherwise   = 10

      newPosBullets :: [Bullet] -> [Enemy] -> GameScore -> ([Bullet],GameScore)
      newPosBullets [] _ gs = ([], gs)
      newPosBullets (b@(MkBullet op np orB vB):bs) es gs
        | fst3 (notCollisionBE b es) == False = newPosBullets bs es newGs
        | distance2 op np > 400 = newPosBullets bs es gs
        | otherwise = (MkBullet op (newPos np orB (vB+5)) orB vB : fst(newPosBullets bs es gs), snd (newPosBullets bs es gs))
        where
            newGs | trd3 (notCollisionBE b es) == 'a' = case () of
                                                             () | snd3 (notCollisionBE b es) == Large -> gs + 20
                                                                | snd3 (notCollisionBE b es) == Med   -> gs + 50
                                                                | otherwise                           -> gs + 100
                  | otherwise  = case () of
                                      ()  | snd3 (notCollisionBE b es) == Large -> gs + 200
                                          | otherwise                           -> gs + 1000

      notCollisionSsAs :: Enemy -> [Enemy] -> Bool
      notCollisionSsAs _ [] = (True)
      notCollisionSsAs s@(Spaceship _ sp _ _) (e@(Asteroid size p orA):es)  | distance2 sp p < dBE size = False
                                                                            | otherwise = True && notCollisionSsAs s es 
      
      notCollisionAsSs :: Enemy -> [Enemy] -> Bool
      notCollisionAsSs _ [] = (True)
      notCollisionAsSs a@(Asteroid _ p _) (e@(Spaceship size sp _ _):es)  | distance2 sp p < dBE size = False
                                                                          | otherwise = True && notCollisionSsAs a es 

      notCollisionBE :: Bullet -> [Enemy] -> (Bool,Size,Char)
      notCollisionBE _ [] = (True, Small,'n')
      notCollisionBE bull@(MkBullet op np orB _) (e@(Asteroid s p orA):es)  | distance2 p np < dBE s = (False,s, 'a')
                                                                            | otherwise = (True && fst3 (notCollisionBE bull es), snd3 $ notCollisionBE bull es, 'a')
      notCollisionBE bull@(MkBullet op np orB _) (e@(Spaceship s p orA _):es) 
                                                                            | distance2 p np < (dBE s)*2/3.5 = (False,s,'s')
                                                                            | otherwise = (True && fst3 (notCollisionBE bull es), snd3 $ notCollisionBE bull es, 's')

      notCollisionEB :: Enemy -> [Bullet] -> Bool
      notCollisionEB _ [] = True
      notCollisionEB enem@(Asteroid s p orA) (b@(MkBullet op np orB _):bs)    | distance2 p np < dBE s = False
                                                                              | otherwise = True && notCollisionEB enem bs
      notCollisionEB enem@(Spaceship s p orA _) (b@(MkBullet op np orB _):bs) | distance2 p np < (dBE s)*2/3.5 = False
                                                                              | otherwise = True && notCollisionEB enem bs

      fst3 (x,_,_) = x
      snd3 (_,y,_) = y
      trd3 (_,_,z) = z

      dBE :: Size -> Float
      dBE s | s == Large = 47
            | s == Med = 28
            | otherwise = 14

      updatedGs = snd $ newPosBullets a enemies gs

      npe | c `mod` 1000 == 0 = fst $ newPosEnemies (fst (mkEnemy rS 's' (x,y) r):enemies) (x,y) a l c
          | otherwise         = fst(newPosEnemies (getSpaceships enemies) (x,y) a l c) ++ fst(newPosEnemies (fst (newPosEnemies (getAsteroids enemies) (x,y) a l c)) (x,y) enemyBullets l c)
          where enemyBullets  = [b | (Spaceship _ _ _ b)<-enemies]


      rS = [Large, Med]!!fst (randomR (0,1) r)

      fire  | c `mod` 5 == 0 = True
            | otherwise = False

      newKeys = delete (SpecialKey KeySpace) ks
      updatedLives = snd $ newPosEnemies enemies (x,y) a l c
      npb art = fst $ newPosBullets art enemies gs

getAsteroids :: [Enemy] -> [Enemy]
getAsteroids es = [a | a@(Asteroid _ _ _)<-es]

getSpaceships :: [Enemy] -> [Enemy]
getSpaceships es = [s | s@(Spaceship _ _ _ _)<-es]

newPos :: (Float, Float) -> Orientation -> Velocity -> (Float, Float)
newPos (x,y) o v  | v == 0 = (x,y)
                  | otherwise = case () of
                                ()  | o >= 0  && o <= 90  -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | o > 90  && o <= 180 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | o > 180 && o <= 270 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | otherwise           -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)


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
handleKeys _ game = game
