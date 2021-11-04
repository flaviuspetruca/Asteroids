module Game where
-- GAME --
-- For all of the game logic not directly linked to input & updating the game.

import Data.List
import Graphics.Gloss
import Data.Number.CReal
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char, SpecialKey),
      KeyState(Up, Down),
      SpecialKey(KeySpace, KeyDelete, KeyEnter),
      Event(EventKey) )
import qualified GHC.Base as S
import System.Random
import GJK.Collision
import GJK.Support
import Graphics.Gloss.Interface.IO.Interact (Key(SpecialKey))

import Struct

ship :: Picture 
ship = Polygon [(17, 0), (-20,15),(-10,0),(-20,-15),(17,0)]

spaceship :: Float -> Picture 
spaceship ss = pictures [Line[(10*ss, 0), (5*ss,-3*ss),(-5*ss,-3*ss),(-10*ss,0),(10*ss,0)],
                      Line [(-10*ss, 0), (-5*ss,3*ss),(5*ss,3*ss),(10*ss,0),(-10*ss,0)],
                      Line [(-3.5*ss, 6*ss), (-5*ss,3*ss),(5*ss,3*ss),(3.5*ss,6*ss),(-3.5*ss,6*ss)]]

asteroid :: (Size->Float) -> Size -> Picture
asteroid f s = Polygon [(-20*f s, 20*f s),(35*f s,0*f s),(20*f s,20*f s),(-15*f s,-15*f s),(15*f s,-15*f s)]

mkPlayer :: Bool -> Color -> Float -> Float -> Float -> Picture
mkPlayer im c x y o | im  = translate x y $ rotate (360-o) $ color c  $ pictures [ship, Polygon [(-19,7),(-35,0),(-19,-7)]]
                    | otherwise = translate x y $ rotate (360-o) $ color c ship

mkEnemy :: Size -> Char -> StdGen -> (Enemy,StdGen)
mkEnemy size c sg {- g@(MkGameState ks s (MkPlayer n gs pos vel l o oo) e a d st p sg) -}
 | c == 'a'     = (Asteroid   size (rx1,ry2) ro, sg3)
 | otherwise  = (Spaceship  size (rx1, ry2) 0, sg3) 
 where  (rx1, sg1)  = randomR (-450,450) sg
        (ry2, sg2)  = randomR (-300,300) sg1
        (ro, sg3)   = randomR (0,360) sg2

mkAsteroids :: Int -> StdGen -> ([Enemy],StdGen)
mkAsteroids 0 sg = ([],sg)
mkAsteroids n sg = (ast : fst (mkAsteroids (n-1) newSg), snd(mkAsteroids (n-1) newSg))
  where (ast, newSg) = mkEnemy Large 'a' sg

createPicture :: Color -> Enemy -> Picture
createPicture c = f
  where f (Asteroid s (x2,y2) o2) = translate x2 y2 $ rotate(360-o2) $ color c $ asteroid size s
        f (Spaceship s (x2,y2) o2) = translate x2 y2 $ color c $ spaceship (size s*1.5)
        size :: Size -> Float
        size s  | s == Large = 2
                | s == Med = 1.1
                | otherwise = 0.5

outOfViewBool :: (Float, Float) -> Float -> Float -> Bool
outOfViewBool (x,y)  w h  | x > w/2.0   = True
                          | y > h/2     = True
                          | x < -w/2    = True
                          | y < -h/2    = True
                          | otherwise   = False

outOfViewCoord :: (Float, Float) -> Float -> Float -> (Float, Float)
outOfViewCoord (x,y) w h  | x > w/2   = (-w/2, y)
                          | y > h/2   = (x, -h/2)
                          | x < -w/2   = (w/2, y)
                          | y < -h/2  = (x, h/2)
                          | otherwise = (x,y)

-- | Update the spaceship movement using its current velocity.
movePlayer :: Float    -- ^ The number of seconds since last update
         -> GameState -- ^ The initial game state
         -> GameState -- ^ A new game state with an updated spaceship movement

movePlayer seconds (MkGameState ks c (MkPlayer n gs im (x,y) vel l o oo) e b _ _ _ r) = newGame
  where newGame | outOfViewBool (x,y) 1000 700  = MkGameState ks c (MkPlayer n gs im (outOfViewCoord (x,y) 1000 700) vel l o oo) e b Easy True False r
                | otherwise                     = MkGameState ks c (MkPlayer n gs im (x,y) vel l o oo) e b Easy True False r

bulletEnemyCollision :: GameState -> GameState
bulletEnemyCollision g@(MkGameState ks c(MkPlayer n gs im (x,y) vel l o oo) es as d st pause r)
 | null es || null as = g
 | otherwise = MkGameState ks c (MkPlayer n gs im (x,y) vel l o oo) newEs newAs d st pause r
  where newEs = [e | e@(Asteroid s p orA)<-es, (MkBullet op np orB _) <-as, collisionBE np p s]
        newAs = [a | (Asteroid s p orA)<-es, a@(MkBullet op np orB _) <-as, collisionBE np p s]
        collisionBE np p s  | distance2 np p > size s = True
                            | otherwise = False
        size s | s == Large  = 20
              | s == Med    = 10
               | otherwise   = 5

createSmallerAsteroids :: Size -> Position -> Orientation -> [Enemy]
createSmallerAsteroids Large (x,y) o = [Asteroid Med (x+70,y) o, Asteroid Med (x,y+70) (360-o)]
createSmallerAsteroids Med (x,y) o = [Asteroid Small (x+50,y-20) o, Asteroid Small(x+20,y+50) (360-o)]
createSmallerAsteroids Small _ _ = []

distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where x' = x1 - x2
          y' = y1 - y2

acc :: Velocity -> Velocity -> Velocity
acc vel newVel | newVel < 0 && (vel + newVel) >= 0   = vel + newVel
                        | newVel < 0 && vel + newVel < 0      = 0
                        | vel + newVel >= 5                   = 5
                        | otherwise                           = vel + newVel

newPos :: (Float, Float) -> Orientation -> Velocity -> (Float, Float)
newPos (x,y) o v  | v == 0 = (x,y)
                  | otherwise = case () of
                                ()  | o >= 0  && o <= 90  -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | o > 90  && o <= 180 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | o > 180 && o <= 270 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
                                    | otherwise           -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)

newOr :: Float -> Float -> Float
newOr o x | (o + x) >= 360 = o + x - 360
          | otherwise = o + x

newPosEnemies :: [Enemy] -> Position -> [Bullet] -> Int -> Int -> StdGen -> Orientation -> ([Enemy],Int)
newPosEnemies [] _ _ lives _ _ _ = ([], lives)
newPosEnemies (e@(Spaceship s p or):es) (x1,y1) bs lives c r o
  | notCollisionEB e bs == False              = newPosEnemies es (x1, y1) bs lives c r o
  | distance2 (x1,y1) (newPos p or 1.5) > dss = (f e : fst (newPosEnemies es (x1,y1) bs lives c r o), newLives)
  | otherwise = (fst $ newPosEnemies es (x1,y1) bs lives c r o, newLives-1)
    where dss | s == Large  = 40
              | s == Med    = 25
              | otherwise   = 10

          f (Spaceship s p or)
            | outOfViewBool (newPos p or enemVel) 1000 700 = Spaceship s (outOfViewCoord (newPos p or enemVel) 1000 700) or
            | otherwise = case () of 
                               () | c `mod` 300 == 0  -> Spaceship s (newPos p (newOr or rO) enemVel) (newOr or rO)
                                  | otherwise        -> Spaceship s (newPos p or enemVel) or

          rO = list!!fst (randomR (0,16) r)
          list = [-315, -270, -225, -180, -135, -90, -45, 0 ,45, 90, 135, 180, 225, 270, 315, 360]

          enemVel | s == Large  = 2.5
                  | s == Med    = 4
                  | otherwise   = 2.5

          newLives = snd (newPosEnemies es (x1,y1) bs lives c r o)
newPosEnemies (e@(Asteroid  s p or):es) (x1,y1) bs lives c r o
  | notCollisionEB e bs == False
    = case  () of
            ()  | s /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives c r o), newLives)
                | otherwise  -> newPosEnemies es (x1,y1) bs lives c r o
  | distance2 (x1,y1) (newPos p or 1.5) > ds  = (f e : fst (newPosEnemies es (x1,y1) bs lives c r o), newLives)
  | otherwise
    = case  () of
            () | s /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives c r o), newLives - 1)
               | otherwise  -> (fst $ newPosEnemies es (x1,y1) bs lives c r o, newLives-1)
    where f (Asteroid s p or) 
            | outOfViewBool (newPos p or enemVel) 1000 700 = Asteroid s (outOfViewCoord (newPos p or enemVel) 1000 700) or
            | otherwise = Asteroid s (newPos p or enemVel) or

          ds | s == Large  = 56
             | s == Med    = 37
             | otherwise   = 20


          enemVel | s == Large  = 1.5
                  | s == Med    = 2
                  | otherwise   = 2.5

          newLives = snd (newPosEnemies es (x1,y1) bs lives c r o)

newPosBullets :: [Bullet] -> [Enemy] -> GameScore -> ([Bullet],GameScore)
newPosBullets [] _ gs = ([], gs)
newPosBullets (b@(MkBullet op np orB vB):bs) es gs
  | fst3 (notCollisionBE b es) == False = newPosBullets bs es newGs
  | distance2 op np > 400 = newPosBullets bs es gs
  | otherwise = (MkBullet op (newPos np orB (vB+5)) orB vB : fst(newPosBullets bs es gs), snd (newPosBullets bs es gs))
    where newGs 
            | trd3 (notCollisionBE b es) == 'a' = case () of 
                                                       () | snd3 (notCollisionBE b es) == Large -> gs + 20
                                                          | snd3 (notCollisionBE b es) == Med   -> gs + 50
                                                          | otherwise                           -> gs + 100
            | otherwise  = case () of
                                ()  | snd3 (notCollisionBE b es) == Large -> gs + 200
                                    | otherwise                           -> gs + 1000

notCollisionBE :: Bullet -> [Enemy] -> (Bool,Size,Char)
notCollisionBE _ [] = (True, Small,'n')
notCollisionBE bull@(MkBullet op np orB _) (e@(Asteroid s p orA):es)  | distance2 p np < dBE s = (False,s, 'a')
                                                                      | otherwise = (True && fst3 (notCollisionBE bull es), snd3 $ notCollisionBE bull es, 'a')
notCollisionBE bull@(MkBullet op np orB _) (e@(Spaceship s p orA):es) | distance2 p np < (dBE s)*2/3.5 = (False,s,'s')
                                                                      | otherwise = (True && fst3 (notCollisionBE bull es), snd3 $ notCollisionBE bull es, 's')

fst3 (x,_,_) = x
snd3 (_,y,_) = y
trd3 (_,_,z) = z

notCollisionEB :: Enemy -> [Bullet] -> Bool
notCollisionEB _ [] = True
notCollisionEB enem@(Asteroid s p orA) (b@(MkBullet op np orB _):bs)  | distance2 p np < dBE s = False
                                                                      | otherwise = True && notCollisionEB enem bs
notCollisionEB enem@(Spaceship s p orA) (b@(MkBullet op np orB _):bs) | distance2 p np < (dBE s)*2/3.5 = False
                                                                      | otherwise = True && notCollisionEB enem bs
dBE :: Size -> Float
dBE s | s == Large = 47
      | s == Med = 28
      | otherwise = 14

