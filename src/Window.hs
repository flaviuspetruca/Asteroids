{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Window where

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

mkEnemy :: Size -> Char -> Position -> StdGen -> (Enemy,StdGen)
mkEnemy size c (x,y) sg {- g@(MkGameState ks s (MkPlayer n gs pos vel l o oo) e a d st p sg) -}
 | c == 'a'     = (Asteroid   size (rx1,ry2) ro, sg3)
 | otherwise  = (Spaceship  size (rx1, ry2) 0 (MkBullet (rx1,ry2) (rx1,ry2) nO 1), sg3) 
 where  (rx1, sg1)  = randomR (-450,450) sg
        (ry2, sg2)  = randomR (-300,300) sg1
        (ro, sg3)   = randomR (0,360) sg2
        nO| x > 0 && y > 0 = acos ((distance2 (rx1,ry2) (x,snd (rx1,ry2)))/(distance2 (rx1,ry2) (x,y))) 
          | x < 0 && y > 0 = 180 - acos ((distance2 (rx1,ry2) (x,snd (rx1,ry2)))/(distance2 (rx1,ry2) (x,y))) 
          | x < 0 && y < 0 = acos ((distance2 (rx1,ry2) (x,snd (rx1,ry2)))/(distance2 (rx1,ry2) (x,y))) 
          | otherwise      = 180 - acos ((distance2 (rx1,ry2) (x,snd (rx1,ry2)))/(distance2 (rx1,ry2) (x,y))) 

mkAsteroids :: Int -> Position -> StdGen -> ([Enemy],StdGen)
mkAsteroids 0 (x,y) sg  = ([],sg)
mkAsteroids n (x,y) sg = (ast : fst (mkAsteroids (n-1) (x,y) newSg), snd(mkAsteroids (n-1) (x,y) newSg))
  where (ast, newSg) = mkEnemy Large 'a' (x,y) sg

createPicture :: Color -> Enemy -> Picture
createPicture c = f
  where f (Asteroid s (x2,y2) o2) = translate x2 y2 $ rotate(360-o2) $ color c $ asteroid size s
        f (Spaceship s (x2,y2) o2 bul) = translate x2 y2 $ color c $ spaceship (size s*1.5)
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

movePlayer seconds (MkGameState ks c (MkPlayer n gs im (x,y) vel l o oo) e b _ hd _ _ r) = newGame
  where newGame | outOfViewBool (x,y) 1000 700  = MkGameState ks c (MkPlayer n gs im (outOfViewCoord (x,y) 1000 700) vel l o oo) e b Easy hd True False r
                | otherwise                     = MkGameState ks c (MkPlayer n gs im (x,y) vel l o oo) e b Easy hd True False r

{- bulletEnemyCollision :: GameState -> GameState
bulletEnemyCollision g@(MkGameState ks c(MkPlayer n gs im (x,y) vel l o oo) es as eb d st pause r)
 | null es || null as = g
 | otherwise = MkGameState ks c (MkPlayer n gs im (x,y) vel l o oo) newEs newAs d st pause r
  where newEs = [e | e@(Asteroid s p orA)<-es, (MkBullet op np orB _) <-as, collisionBE np p s]
        newAs = [a | (Asteroid s p orA)<-es, a@(MkBullet op np orB _) <-as, collisionBE np p s]
        collisionBE np p s  | distance2 np p > size s = True
                            | otherwise = False
        size s | s == Large  = 20
               | s == Med    = 10
               | otherwise   = 5 -}

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
