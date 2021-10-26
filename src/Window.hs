module Window where

import Data.List
import Graphics.Gloss
import Data.Number.CReal
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char), KeyState(Up, Down), Event(EventKey) )
import qualified GHC.Base as S
import System.Random
import GJK.Collision
import GJK.Support

--import Menu
--import Logic

import Struct

--main :: IO ()
--main = undefined
--main = play window background fps initialState render handleKeys update

--initialState :: GameState
--initialState = MkGameState
--  {
--    keys = [],
--    score = 0,
--    player = MkPlayer{
--      pName = "Flavius",
--      gameScore = 0,
--      position = (0,0),
--      velocity = 0.0,
--      lives = 3,
--      orientation = 90,
--      oldOrientation = 90
--    },
--    enemies = fst $ mkAsteroids 10 (mkStdGen 2),
--    allArtilery = [(1,1,1), (1,2,2)],
--    difficulty = Easy,
--    started = True, --should be false(going to change it after we create the menu)
--    paused = False,
--    sg = snd $ mkAsteroids 10 (mkStdGen 2)
--  }

mkPlayer :: Color -> Float -> Float -> Float -> Picture
mkPlayer c x y o = translate x y $ rotate (360-o) $ color c  $ Polygon [(15, 0), (-15,10),(-10,0),(-15,-10),(15,0)]

mkAsteroid :: Size -> StdGen -> (Enemy,StdGen)
mkAsteroid size sg {- g@(MkGameState ks s (MkPlayer n gs pos vel l o oo) e a d st p sg) -}
 = (Asteroid size (rx1,ry2) ro, sg3)
 where  (rx1, sg1)  = randomR (-450,450) sg
        (ry2, sg2)  = randomR (-300,300) sg1
        (ro, sg3)   = randomR (0,360) sg2

mkAsteroids :: Int -> StdGen -> ([Enemy],StdGen)
mkAsteroids 0 sg = ([],sg)
mkAsteroids n sg = (ast : fst (mkAsteroids (n-1) newSg), snd(mkAsteroids (n-1) newSg))
  where (ast, newSg) = mkAsteroid Med sg

createPicture :: Color -> Enemy -> Picture
createPicture c = f
  where f (Asteroid s (x2,y2) o2) = translate x2 y2 $ rotate(360-o2) $ color c $ Polygon [(-20*size s, 20*size s),(35*size s,0*size s),(20*size s,20*size s),(-15*size s,-15*size s),(15*size s,-15*size s)]
        f (Spaceship s (x2,y2) o2) = translate x2 y2 $ rotate(360-o2) $ color c $ Circle 200
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
-- | Convert a game state into a picture.
--render :: GameState -> Picture
--render (MkGameState ks _ (MkPlayer _ _ (x,y) _ _ o oo) en _ _ _ _ _ )
--      = pictures (mkPlayer white x y o:enemies)
--      where enemies = map (createPicture white) en

-- | Update the spaceship movement using its current velocity.
movePlayer :: Float    -- ^ The number of seconds since last update
         -> GameState -- ^ The initial game state
         -> GameState -- ^ A new game state with an updated spaceship movement
movePlayer seconds (MkGameState ks s (MkPlayer n _ (x,y) vel _ o oo) e _ _ _ _ r) = newGame
  where newGame | outOfViewBool (x,y) 900 600 = MkGameState ks s (MkPlayer n 0 (outOfViewCoord (x,y) 900 600) vel 3 o oo) e [] Easy True False r
                | otherwise                   = MkGameState ks s (MkPlayer n 0 (x,y) vel 3 o oo) e [] Easy True False r

--update ::  Float -> GameState -> GameState
--update sec (MkGameState ks s (MkPlayer n gs (x,y) vel l o oo) e a d st p r)
--  | Char 'w' `elem` ks && Char 'a' `elem` ks = if oo /= o && vel > 0
--                                                then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.05)) l (newOr o    5) (newOr o   5)) npe a d st p r)
--                                                else movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel 0.3) l (newOr o    5) (newOr o   5)) npe a d st p r)
--  | Char 'w' `elem` ks && Char 'd' `elem` ks = if oo /= o && vel > 0
--                                                then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.05)) l (newOr o (-5)) (newOr o   5)) npe a d st p r)
--                                                else movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel 0.3) l (newOr o (-5)) (newOr o   5)) npe a d st p r)
--  | Char 'w' `elem` ks = if oo/=o && vel > 0
--                          then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) o vel) (acceleration vel (-0.1)) l o o) npe a d st p r)
--                          else movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) o vel) (acceleration vel 0.3) l o o) npe a d st p r)
--  | Char 'a' `elem` ks = if vel > 0
--                            then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.07)) l (newOr o   5) oo) npe a d st p r)
--                            else movePlayer sec (MkGameState ks s (MkPlayer n gs (x,y) vel l (newOr o   5) oo) npe a d st p r)
--  | Char 'd' `elem` ks = if vel > 0
--                            then movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.07)) l (newOr o (-5)) oo) npe a d st p r)
--                            else movePlayer sec (MkGameState ks s (MkPlayer n gs (x,y) vel l (newOr o (-5)) oo) npe a d st p r)
--  | otherwise          = movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) oo vel) (acceleration vel (-0.07)) l o oo) npe a d st p r)
--  where
--        newOr :: Float -> Float -> Float
--        newOr o x | (o + x) >= 360 = o + x - 360
--                  | otherwise = o + x
--        newPos :: (Float, Float) -> Orientation -> Velocity -> (Float, Float)
--        newPos (x,y) o v  | v == 0 = (x,y)
--                          | otherwise = case () of
--                                        ()  | o >= 0  && o <= 90  -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
--                                            | o > 90  && o <= 180 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
--                                            | o > 180 && o <= 270 -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
--                                            | otherwise           -> (x + cos(o*pi/180)*v, y + sin(o*pi/180)*v)
--        newPosEnemies :: [Enemy] -> Position -> [Enemy]
--        newPosEnemies es (x1,y1) = map f es
--          where f (Asteroid s p or) | distance2 (x1,y1) (newPos p or 1.5) < 37 = Spaceship Small (0,0) 0
--                                    | outOfViewBool (newPos p or 1.5) 900 600 = Asteroid s (outOfViewCoord (newPos p or 1.5) 900 600) or
--                                    | otherwise = Asteroid s (newPos p or 1.5) or
--                f sp = sp
        
--        npe = newPosEnemies e (x,y)

distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where x' = x1 - x2
          y' = y1 - y2

findCol :: [(Double,Double)] -> [(Double,Double)] -> Bool
findCol a b | collision 10 (a, polySupport) (b, polySupport) == Just True = True
            | otherwise = False


acceleration :: Velocity -> Velocity -> Velocity
acceleration vel newVel | newVel < 0 && (vel + newVel) >= 0   = vel + newVel
                        | newVel < 0 && vel + newVel < 0      = 0
                        | vel + newVel >= 5                   = 5
                        | otherwise                           = vel + newVel

--handleKeys :: Event-> GameState -> GameState
--handleKeys (EventKey key@(Char c) state _ _) g@(MkGameState ks s (MkPlayer n gs (x,y) vel l o oo) e a d st p r)
--  | c == 'w' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
--                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
--  | c == 'a' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
--                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
--  | c == 'd' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
--                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
--handleKeys _ game = game
