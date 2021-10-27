module Window(main) where

import Data.List
import Graphics.Gloss
import Data.Number.CReal
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char, SpecialKey),
      KeyState(Up, Down),
      SpecialKey(KeySpace),
      Event(EventKey) )
import qualified GHC.Base as S
import System.Random
import GJK.Collision
import GJK.Support
import Graphics.Gloss.Interface.IO.Interact (Key(SpecialKey))

width, height, offset :: Int
width = 900
height = 600
offset = 50

fps :: Int
fps = 60

window :: Display
window = InWindow "Asteroids" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = play window background fps initialState render handleKeys update{- New
 where updateNew sec g = update sec (bulletEnemyCollision g) -}


type Position = (Float, Float)
type Name  = String
type GameScore = Int
type Orientation = Float
type Velocity = Float
type Lives = Int
data Difficulty = Hard | Medium | Easy
data Size = Large | Med | Small deriving(Eq)
data Bullet = MkBullet Position Position Orientation Velocity

data Player = MkPlayer {
  name ::Name,
  gameScore :: GameScore,
  position :: Position,
  velocity :: Velocity,
  lives :: Lives,
  orientation :: Orientation,
  oldOrientation :: Orientation
}

data GameState = MkGameState {
  keys :: [Key],
  score :: GameScore,
  player :: Player,
  enemies :: [Enemy],
  allArtilery :: [Bullet],
  difficulty :: Difficulty,
  started :: Bool,
  paused :: Bool,
  sg :: StdGen
}

instance Show GameState where
  show (MkGameState ks s(MkPlayer n gs pos vel l o oo) enemies a d st p sg) = show sg

data Enemy =
  Asteroid Size Position Orientation|
  Spaceship Size Position Orientation

instance Show Enemy where
  show (Asteroid s p o) = show p ++ show o
  show Spaceship {} = show 2

data Movement = MkMovement {
  pos :: Position,
  orient :: Orientation,
  vel :: Velocity
}

initialState :: GameState
initialState = MkGameState
  {
    keys = [],
    score = 0,
    player = MkPlayer{
      name = "Flavius",
      gameScore = 0,
      position = (0,0),
      velocity = 0.0,
      lives = 3,
      orientation = 90,
      oldOrientation = 90
    },
    enemies = fst $ mkAsteroids 5 (mkStdGen 2),
    allArtilery = [],
    difficulty = Easy,
    started = True, --should be false(going to change it after we create the menu)
    paused = False,
    sg = snd $ mkAsteroids 5 (mkStdGen 2)
  }

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
  where (ast, newSg) = mkAsteroid Large sg

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
render :: GameState -> Picture
render (MkGameState ks _ (MkPlayer _ _ (x,y) _ l o oo) en b _ _ _ _ )
      | l > 0 = pictures (mkPlayer white x y o:(enemies++bullets))
      | otherwise = pictures [ color white $ Circle 20]
      where enemies = map (createPicture white) en
            bullets = map (\(MkBullet op (newX, newY) o _) -> translate newX newY $ rotate (360-o) $ color white $ ThickCircle 2.5 5) b

-- | Update the spaceship movement using its current velocity.
movePlayer :: Float    -- ^ The number of seconds since last update
         -> GameState -- ^ The initial game state
         -> GameState -- ^ A new game state with an updated spaceship movement
movePlayer seconds (MkGameState ks s (MkPlayer _ _ (x,y) vel l o oo) e b _ _ _ r) = newGame
  where newGame | outOfViewBool (x,y) 1000 700  = MkGameState ks s (MkPlayer "Flavius" 0 (outOfViewCoord (x,y) 1000 700) vel l o oo) e b Easy True False r
                | otherwise                     = MkGameState ks s (MkPlayer "Flavius" 0 (x,y) vel l o oo) e b Easy True False r

update ::  Float -> GameState -> GameState
update sec g@(MkGameState ks s (MkPlayer n gs (x,y) vel l o oo) e a d st p r)
  | SpecialKey KeySpace `elem` ks && Char 'w' `elem` ks && Char 'd' `elem` ks
    = case  () of
            () | oo/=o && vel > 0 -> movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.05)) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb (MkBullet (x,y) (x,y) o vel:a) e gs) d st p r)
               | otherwise        -> movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel 0.3) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb (MkBullet (x,y) (x,y) o vel:a)e gs) d st p r)
  | SpecialKey KeySpace `elem` ks && Char 'w' `elem` ks && Char 'a' `elem` ks
    = case  () of
            () | oo/=o && vel > 0 -> movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.05)) updatedLives (newOr o 3) (newOr o  (-3))) npe (npb (MkBullet (x,y) (x,y) o vel:a) e gs) d st p r)
               | otherwise        -> movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel 0.3) updatedLives (newOr o 3) (newOr o (-3))) npe (npb (MkBullet (x,y) (x,y) o vel:a)e gs) d st p r)

  | Char 'w' `elem` ks && Char 'd' `elem` ks = if oo /= o && vel > 0
                                                then movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.05)) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb a e gs) d st p r)
                                                else movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel 0.3) updatedLives (newOr o (-3)) (newOr o   3)) npe (npb a e gs) d st p r)
  | Char 'w' `elem` ks && Char 'a' `elem` ks = if oo /= o && vel > 0
                                                then movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.05)) updatedLives (newOr o    3) (newOr o   (-3))) npe (npb a e gs) d st p r)
                                                else movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel 0.3) updatedLives (newOr o    3) (newOr o   (-3))) npe (npb a e gs) d st p r)

  | SpecialKey KeySpace `elem` ks && Char 'w' `elem` ks = case () of
                                                          () | oo/=o && vel > 0 -> movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) o vel) (acceleration vel (-0.1)) updatedLives o o) npe (npb (MkBullet (x,y) (x,y) o vel:a) e gs) d st p r)
                                                             | otherwise        -> movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) o vel) (acceleration vel 0.3) updatedLives o o) npe (npb (MkBullet (x,y) (x,y) o vel:a) e gs) d st p r)

  | SpecialKey KeySpace `elem` ks && Char 'a' `elem` ks = movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.07)) updatedLives (newOr o 3) oo) npe (npb (MkBullet (x,y) (x,y) o vel:a) e gs) d st p r)
  | SpecialKey KeySpace `elem` ks && Char 'd' `elem` ks = movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.07)) updatedLives (newOr o (-3)) oo) npe (npb (MkBullet (x,y) (x,y) o vel:a) e gs) d st p r)
  | SpecialKey KeySpace `elem` ks = movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.07)) updatedLives o oo) npe (npb (MkBullet (x,y) (x,y) o vel:a) e gs) d st p r)

  | Char 'w' `elem` ks = if oo/=o && vel > 0
                          then movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) o vel) (acceleration vel (-0.1)) updatedLives o o) npe (npb a e gs) d st p r)
                          else movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) o vel) (acceleration vel 0.3) updatedLives o o) npe (npb a e gs) d st p r)
  | Char 'a' `elem` ks = if vel > 0
                          then movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.07)) updatedLives (newOr o   3) oo) npe (npb a e gs) d st p r)
                          else movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (x,y) vel updatedLives (newOr o   5) oo) npe (npb a e gs) d st p r)
  | Char 'd' `elem` ks = if vel > 0
                            then movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.07)) updatedLives (newOr o (-3)) oo) npe (npb a e gs) d st p r)
                            else movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (x,y) vel updatedLives (newOr o (-3)) oo) npe (npb a e gs) d st p r)
  | otherwise          = movePlayer sec (MkGameState ks s (MkPlayer n (updatedGs a e gs) (newPos (x,y) oo vel) (acceleration vel (-0.07)) updatedLives o oo) npe (npb a e gs) d st p r)
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

        newPosEnemies :: [Enemy] -> Position -> [Bullet] -> Int -> ([Enemy],Int)
        newPosEnemies [] _ _ lives = ([], lives)
        newPosEnemies (e@Spaceship{}:es) _ _ lives= ([], lives)
        newPosEnemies (e@(Asteroid s p or):es) (x1,y1) bs lives
          | notCollisionEB e bs == False = case () of
            ()  | s /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives), newLives)
                | otherwise  -> newPosEnemies es (x1,y1) bs lives
          | distance2 (x1,y1) (newPos p or 1.5) > ds  = (f e : fst (newPosEnemies es (x1,y1) bs lives), newLives)
          | otherwise = case () of
            () | s /= Small -> (createSmallerAsteroids s p o ++ fst (newPosEnemies es (x1,y1) bs lives), newLives - 1)
               | otherwise  -> (fst $ newPosEnemies es (x1,y1) bs lives, newLives-1)
          where f (Asteroid s p or) | outOfViewBool (newPos p or enemVel) 1000 700 = Asteroid s (outOfViewCoord (newPos p or enemVel) 1000 700) or
                                    | otherwise = Asteroid s (newPos p or enemVel) or
                f sp = sp
                ds  | s == Large  = 43
                    | s == Med    = 37
                    | otherwise   = 20

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
        
        newGs astSize | astSize == Large = gs + 20
                      | astSize == Med   = gs + 50
                      | otherwise  = gs + 100

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
        dBE s | s == Large = 30
              | s == Med = 20
              | otherwise = 10

        updatedGs art ene gs = snd $ newPosBullets art ene gs
        npe = fst $ newPosEnemies e (x,y) a l
        updatedLives = snd $ newPosEnemies e (x,y) a l
        npb art ene gs= fst $ newPosBullets art ene gs

bulletEnemyCollision :: GameState -> GameState
bulletEnemyCollision g@(MkGameState ks sc (MkPlayer n gs (x,y) vel l o oo) es as d st pause r)
 | null es || null as = g
 | otherwise = MkGameState ks sc (MkPlayer n gs (x,y) vel l o oo) newEs newAs d st pause r
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

acceleration :: Velocity -> Velocity -> Velocity
acceleration vel newVel | newVel < 0 && (vel + newVel) >= 0   = vel + newVel
                        | newVel < 0 && vel + newVel < 0      = 0
                        | vel + newVel >= 5                   = 5
                        | otherwise                           = vel + newVel

handleKeys :: Event-> GameState -> GameState
handleKeys (EventKey key@(Char c) state _ _) g@(MkGameState ks s (MkPlayer n gs (x,y) vel l o oo) e a d st p r)
  | c == 'w' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
  | c == 'a' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
  | c == 'd' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
handleKeys (EventKey key@(SpecialKey KeySpace) state _ _)  g@(MkGameState ks s (MkPlayer n gs (x,y) vel l o oo) e a d st p r)
          | state == Down = MkGameState (insert key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
          | otherwise     = MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o oo) e a d st p r
handleKeys _ game = game
