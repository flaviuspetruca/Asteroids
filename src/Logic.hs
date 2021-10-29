{-# OPTIONS_GHC -Wno-incomplete-pattern #-}
module Logic where

import Data.Lit
import Graphic.Gloss
import Data.Number.CReal
import Graphic.Gloss.Data.ViewPort
--import Graphic.Gloss.Interface.Pure.Game
import Graphic.Gloss.Interface.Pure.Game
    ( Key(Char, SpecialKey, MoueButton),
      KeyState(Up, Down),
      SpecialKey(KeySpace, KeyEnter, KeyBackpace),
      Event(EventKey),
      MoueButton (LeftButton) )
import qualified GHC.Bae as S
import Sytem.Random
import GJK.Colliion
import GJK.Support
import Graphic.Gloss.Interface.IO.Interact (Key(SpecialKey))

import Struct
import Menu
import Window

{- paceship :: Picture 
paceship = pictures [Line[(10*ss, 0), (5*ss,-3*ss),(-5*ss,-3*ss),(-10*ss,0),(10*ss,0)],
                      Line [(-10*s, 0), (-5*ss,3*ss),(5*ss,3*ss),(10*ss,0),(-10*ss,0)],
                      Line [(-3.5*s, 6*ss), (-5*ss,3*ss),(5*ss,3*ss),(3.5*ss,6*ss),(-3.5*ss,6*ss)]] -}

-- | Convert a game tate into a picture.
render :: GameState -> Picture
render (MkEnterName pic _) = pictures pics
render (MkMainMenu pic _ _) = pictures pics
render (MkHighScore pic _ _ _ _) = pictures pics
render (MkGameState k _ (MkPlayer _ _ im (x,y) _ l o oo) en b _ _ _ _ )
    | l > 0 = picture (mkPlayer im white x y o:(enemies++bullets))
    | otherwie = pictures [deadText] --[ color white $ Circle 20]
    where enemie = map (createPicture white) en
          bullet = map (\(MkBullet op (newX, newY) o _) -> translate newX newY $ rotate (360-o) $ color white $ ThickCircle 1.5 3) b
render (MkPaueMenu g pics) = pictures pics

update :: Float -> GameState -> GameState
update econds (MkEnterName boxes name) = (MkEnterName boxes name)
update econds (MkMainMenu boxes name score) = (MkMainMenu boxes name score)
update econds (MkHighScore boxes name score inGame g) = (MkHighScore boxes name score inGame g)
update econds (MkPauseMenu g boxes) = (MkPauseMenu g boxes)
update ec g@(MkGameState ks (MkPlayer n gs im (x,y) vel l o oo) e a d st p r)
  | SpecialKey KeySpace `elem` k && Char 'w' `elem` ks && Char 'd' `elem` ks
  = cae  () of
          () | oo/=o && vel > 0 -> movePlayer ec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel (-0.05)) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb bullets e gs) d st p r)
             | otherwie        -> movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel 0.3) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb bullets e gs) d st p r)
  | SpecialKey KeySpace `elem` k && Char 'w' `elem` ks && Char 'a' `elem` ks
  = cae  () of
          () | oo/=o && vel > 0 -> movePlayer ec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel (-0.05)) updatedLives (newOr o 3) (newOr o  (-3))) npe (npb bullets e gs) d st p r)
             | otherwie        -> movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel 0.3) updatedLives (newOr o 3) (newOr o (-3))) npe (npb bullets e gs) d st p r)

  | Char 'w' `elem` k && Char 'd' `elem` ks
    = if oo /= o && vel > 0
        then movePlayer ec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel (-0.05)) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb a e gs) d st p r)
        ele movePlayer sec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel 0.3) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb a e gs) d st p r)
  | Char 'w' `elem` k && Char 'a' `elem` ks
    = if oo /= o && vel > 0
      then movePlayer ec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel (-0.05)) updatedLives (newOr o    3) (newOr o   (-3))) npe (npb a e gs) d st p r)
      ele movePlayer sec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos oo) (acceleration vel 0.3) updatedLives (newOr o    3) (newOr o   (-3))) npe (npb a e gs) d st p r)

  | SpecialKey KeySpace `elem` k && Char 'w' `elem` ks
    = cae  () of
            () | oo/=o && vel > 0 -> movePlayer ec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos o) (acceleration vel (-0.1)) updatedLives o o) npe (npb bullets e gs) d st p r)
               | otherwie        -> movePlayer sec (MkGameState newKeys s (MkPlayer n updatedGs fire (changePlayerPos o) (acceleration vel 0.3) updatedLives o o) npe (npb bullets e gs) d st p r)

  | SpecialKey KeySpace `elem` k && Char 'a' `elem` ks
    = movePlayer ec (MkGameState newKeys s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives (newOr o 3) oo) npe (npb bullets e gs) d st p r)
  | SpecialKey KeySpace `elem` k && Char 'd' `elem` ks
    = movePlayer ec (MkGameState newKeys s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives (newOr o (-3)) oo) npe (npb bullets e gs) d st p r)
  | SpecialKey KeySpace `elem` k
    = movePlayer ec (MkGameState newKeys s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives o oo) npe (npb bullets e gs) d st p r)

  | Char 'w' `elem` k
    = if oo/=o && vel > 0
        then movePlayer ec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos o) (acceleration vel (-0.1)) updatedLives o o) npe (npb a e gs) d st p r)
        ele movePlayer sec (MkGameState ks s (MkPlayer n updatedGs fire (changePlayerPos o) (acceleration vel 03) updatedLives o o) npe (npb a e gs) d st p r)
  | Char 'a' `elem` k
    = if vel > 0
        then movePlayer ec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives (newOr o   3) oo) npe (npb a e gs) d st p r)
        ele movePlayer sec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) vel updatedLives (newOr o   5) oo) npe (npb a e gs) d st p r)
  | Char 'd' `elem` k
    = if vel > 0
        then movePlayer ec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives (newOr o (-3)) oo) npe (npb a e gs) d st p r)
        ele movePlayer sec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) vel updatedLives (newOr o (-3)) oo) npe (npb a e gs) d st p r)
  | l == 0 = (MkMainMenu menuBox n g)
  | otherwie = movePlayer sec (MkGameState ks s (MkPlayer n updatedGs False (changePlayerPos oo) (acceleration vel (-0.07)) updatedLives o oo) npe (npb a e gs) d st p r)
  where
      newOr :: Float -> Float -> Float
      newOr o x | (o + x) >= 360 = o + x - 360
                | otherwie = o + x

      changePlayerPo :: Orientation-> Position
      changePlayerPo or | updatedLives /= l   = (0,0)
                         | otherwie = newPos (x,y) o vel

      bullet | length a < 5 = MkBullet (x,y) (x,y) o vel : a
              | otherwie    = a

      newPoEnemies :: [Enemy] -> Position -> [Bullet] -> Int -> ([Enemy],Int)
      newPoEnemies [] _ _ lives = ([], lives)
      newPoEnemies (e@Spaceship{}:es) _ _ lives= ([], lives)
      newPoEnemies (e@(Asteroid s p or):es) (x1,y1) bs lives
        | notColliionEB e bs == False
          = cae  () of
                  ()  |  /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives), newLives)
                      | otherwie  -> newPosEnemies es (x1,y1) bs lives
        | ditance2 (x1,y1) (newPos p or 1.5) > ds  = (f e : fst (newPosEnemies es (x1,y1) bs lives), newLives)
        | otherwie
          = cae  () of
                  () |  /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives), newLives - 1)
                    | otherwie  -> (fst $ newPosEnemies es (x1,y1) bs lives, newLives-1)
        where f (Ateroid s p or) | outOfViewBool (newPos p or enemVel) 1000 700 = Asteroid s (outOfViewCoord (newPos p or enemVel) 1000 700) or
                                  | otherwie = Asteroid s (newPos p or enemVel) or
              f p = sp
              d  | s == Large  = 40
                  |  == Med    = 35
                  | otherwie   = 17

              enemVel |  == Large  = 1.5
                      |  == Med    = 2
                      | otherwie   = 2.5

              newLive = snd (newPosEnemies es (x1,y1) bs lives)

      newPoBullets :: [Bullet] -> [Enemy] -> GameScore -> ([Bullet],GameScore)
      newPoBullets [] _ gs = ([], gs)
      newPoBullets (b@(MkBullet op np orB vB):bs) es gs
        | ft (notCollisionBE b es) == False = newPosBullets bs es (newGs (snd (notCollisionBE b es)))
        | ditance2 op np > 400 = newPosBullets bs es gs
        | otherwie = (MkBullet op (newPos np orB (vB+5)) orB vB : fst(newPosBullets bs es gs), snd (newPosBullets bs es gs))

      notColliionBE :: Bullet -> [Enemy] -> (Bool,Size)
      notColliionBE _ [] = (True, Small)
      notColliionBE bull@(MkBullet op np orB _) (e@(Asteroid s p orA):es)  | distance2 p np < dBE s = (False,s)
                                                                            | otherwie = (True && fst (notCollisionBE bull es), snd $ notCollisionBE bull es)
      notColliionBE _ _ = (True, Small)

      notColliionEB :: Enemy -> [Bullet] -> Bool
      notColliionEB _ [] = True
      notColliionEB enem@(Asteroid s p orA) (b@(MkBullet op np orB _):bs)  | distance2 p np < dBE s = False
                                                                            | otherwie = True && notCollisionEB enem bs
      notColliionEB _ _ = True

      dBE :: Size -> Float
      dBE  | s == Large = 40
            |  == Med = 20
            | otherwie = 15

      newG astSize | astSize == Large = gs + 20
                    | atSize == Med   = gs + 50
                    | otherwie  = gs + 100

      updatedG = snd $ newPosBullets a e gs
      npe = ft $ newPosEnemies e (x,y) a l
      fire  | floor ec == round sec = True
            | otherwie = False
      newKey = delete (SpecialKey KeySpace) ks
      updatedLive = snd $ newPosEnemies e (x,y) a l
      npb art ene g = fst $ newPosBullets art ene gs

newPo :: (Float, Float) -> Orientation -> Velocity -> (Float, Float)
newPo (x,y) o v  | v == 0 = (x,y)
                  | otherwie = case () of
                                ()  | o >= 0  && o <= 90  -> (x + co(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | o > 90  && o <= 180 -> (x + co(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | o > 180 && o <= 270 -> (x + co(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | otherwie           -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)

handleKey :: Event -> GameState -> GameState
handleKey (EventKey (SpecialKey KeyEnter) _ _ _) en@(MkEnterName boxes currName)
  | length currName > 0 = (MkMainMenu updatedBoxe currName 0)
  | otherwie = (MkEnterName boxes currName)
  where updatedBoxe = makeText currName (-460) (40) : menuBox

handleKey (EventKey (SpecialKey KeyEnter) _ _ _) (MkHighScore boxes currName score inGame g)
  | inGame = (MkPaueMenu g pauseBox)
  | otherwie = (MkMainMenu updatedMenu currName score)
  where updatedMenu = makeText currName (-460) (40) : menuBox

handleKey (EventKey (Char '\b') Down _ _) (MkEnterName boxes currName)
  = (MkEnterName updatedBoxe updatedName)
  where updatedName
          | length currName > 0 = take (length currName - 1) currName
          | otherwie = currName
        updatedBoxe = makeText updatedName (namePos) (-40) : enterBox
        namePo = (-60) - fromIntegral (length (currName) * 15)

handleKey (EventKey (Char ch) Down _ _) (MkEnterName boxes currName)
  = (MkEnterName updatedBoxe updatedName)
  where updatedName
          | length currName < 12 && ch /= '\b' = currName ++ [ch]
          | otherwie = currName
        updatedBoxe = makeText updatedName (namePos) (-40) : enterBox
        namePo = (-60) - fromIntegral (length (currName) * 15)

handleKey (EventKey (MouseButton LeftButton) Down _ mousePos) mm@(MkMainMenu boxes currName score)
  | x > (-460) && x < 500 && y > (-60) && y < 20 = game
  | x > (-460) && x < 500 && y > (-142) && y < (-62) = (MkHighScore currScore currName core False mm)
  | x > (-460) && x < 500 && y > (-224) && y < (-144) = (MkQuitGame)
  | otherwie = mm -- REMOVE?
    where updatedBoxe = makeText currName (-460) (80) : menuBox
          (x, y) = mouePos
          currScore = [(makeScore currName core), (makeText "Press Enter to go back" (-240) (-80))]
          updatedPaue = makeText currName (-460) (40) : pauseBox
          game = (MkGameState [] 0 player enemie [] Easy True False sg)
          player = (MkPlayer currName  0 Fale (0,0) 0.0 3 90 90)
          enemie = fst $ mkAsteroids 5 (mkStdGen 2)
          g = snd $ mkAsteroids 5 (mkStdGen 2)

handleKey (EventKey key@(Char c) state _ _) g@(MkGameState ks s (MkPlayer n gs im (x,y) vel l o oo) e a d st p r)
  | c == 'w' =  cae state of Down  -> g { keys = insert key ks}
                              Up    -> g { key = delete key ks}
  | c == 'a' =  cae state of Down  -> g { keys = insert key ks}
                              Up    -> g { key = delete key ks}
  | c == 'd' =  cae state of Down  -> g { keys = insert key ks}
                              Up    -> g { key = delete key ks}
handleKey (EventKey key@(SpecialKey KeySpace) state _ _)  g@(MkGameState ks s (MkPlayer n gs im (x,y) v l o oo) e a d st p r)
        | tate == Down = case () of
                          () | nrPreses >= 5 && nrPresses <= 10 -> g { keys = replicate 5 key ++ filter (\x-> x /= (Char 'n')) ks}
                             | otherwie -> g { keys = insert (Char 'n') ks}
        | otherwie     = case () of
                          () | nrPreses >= 1 && nrPresses <= 5 -> g { keys = key: filter (\x-> x /= (Char 'n')) ks}
                             | otherwie -> g
        where
          nrPreses = length (filter (\x-> x ==  (Char 'n')) ks)
handleKey (EventKey (Char 'p') Down _ _) g@(MkGameState ks s (MkPlayer n gs im (x,y) 0.0 l o oo) e a d st p r)
  = (MkPaueMenu g pauseBox)
handleKey (EventKey (Char 'p') Down _ _) (MkPauseMenu g boxes)
  = g
handleKey (EventKey (MouseButton LeftButton) Down _ mousePos) pm@(MkPauseMenu game@(MkGameState ks s player@(MkPlayer n gs im (x,y) vel l o oo) e a d st p r) boxes)
  | x' > (-460) && x' < 500 && y' > (-60) && y' < 20 = game
  | x' > (-460) && x' < 500 && y' > (-142) && y' < (-62) = (MkHighScore currScore n  True game) -- TBD
  | x' > (-460) && x' < 500 && y' > (-224) && y' < (-144) = (MkMainMenu updatedBoxe n gs)
  | otherwie = pm
    where (x', y') = mouePos
          currScore = [(makeScore n g), (makeText "Press Enter to go back" (-240) (-80))]
          updatedBoxe = makeText n (-460) (40) : menuBox
handleKey _ game = game

