module Window(main) where

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char), KeyState(Up, Down), Event(EventKey) )
import qualified GHC.Base as S

width, height, offset :: Int
width = 700
height = 400
offset = 100

fps :: Int
fps = 60

window :: Display
window = InWindow "Asteroids" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = play window background fps initialState render handleKeys update

--making the spaceship

type Position = (Float, Float)
type Name  = String
type GameScore = Int
type Orientation = Float
type Velocity = Float
type Lives = Int
data Difficulty = Hard | Medium | Easy
type Bullet = (Float, Float, Float)

data Player = MkPlayer {
  name ::Name,
  gameScore :: GameScore,
  position :: Position,
  velocity :: Velocity,
  lives :: Lives,
  orientation :: Orientation
}

data GameState = MkGameState {
  keys :: [Key],
  score :: GameScore,
  player :: Player,
  enemies :: [Enemy],
  allArtilery :: [Bullet],
  difficulty :: Difficulty,
  started :: Bool,
  paused :: Bool
}

data Enemy =  
  Asteroid |
  Spaceship
  
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
      orientation = 270
    },
    enemies = [Asteroid, Asteroid, Asteroid],
    allArtilery = [(1,1,1), (1,2,2)],
    difficulty = Easy,
    started = True, --should be false(going to change it after we create the menu)
    paused = False
  }

mkPlayer :: Color -> Float -> Float -> Float -> Picture 
mkPlayer c x y o = translate x y $ rotate o $ color c  $ Polygon [(15, 0), (-15,10),(-10,0),(-15,-10),(15,0)]

-- | Convert a game state into a picture.
render :: GameState -> Picture
render (MkGameState ks _ (MkPlayer _ _ (x,y) _ _ o) _ _ _ _ _) = pictures [mkPlayer white x y o]

-- | Update the spaceship movement using its current velocity.
movePlayer :: Float    -- ^ The number of seconds since last update
         -> GameState -- ^ The initial game state
         -> GameState -- ^ A new game state with an updated spaceship movement
movePlayer seconds (MkGameState ks s (MkPlayer _ _ (x,y) vel _ o) _ _ _ _ _) = newGame
  where newGame = MkGameState ks s (MkPlayer "Flavius" 0 (x,y) vel 3 o) [Asteroid] [] Easy True False 


update ::  Float -> GameState -> GameState
update sec (MkGameState ks s (MkPlayer n gs (x,y) vel l o) e a d st p) 
  | Char 'w' `elem` ks = movePlayer sec (MkGameState ks s (MkPlayer n gs (newPos (x,y) o vel) (acceleration vel (-0.1)) l o) e a d st p)
  | Char 'a' `elem` ks = movePlayer sec (MkGameState ks s (MkPlayer n gs (x,y) vel l (newOr o (-5))) e a d st p)
  | Char 'd' `elem` ks = movePlayer sec (MkGameState ks s (MkPlayer n gs (x,y) vel l (newOr o   5)) e a d st p)
  | otherwise          = movePlayer sec (MkGameState ks s (MkPlayer n gs (x,y) (acceleration vel (-0.1)) l o) e a d st p)
  where 
        newOr :: Float -> Float -> Float 
        newOr o x | o + x > 360 = o + x - 360
                  | otherwise = o + x
        newPos :: (Float, Float) -> Orientation -> Velocity -> (Float, Float)
        newPos (x,y) o v  | v == 0 = (x,y)
                          | otherwise = case () of
                                        ()  | o >= 0  && o <= 90  -> (x, y+o/10)
                                            | o > 90  && o <= 180 -> (x-(180-o)/10, y+(o-90)/10)
                                            | o > 180 && o <= 270 -> (x-(270-o)/10, y-(o-180)/10)
                                            | otherwise           -> (x+(o-270)/10, y-(360-o)/10)

{- mkMove :: Key -> Velocity -> Position -> Position
mkMove (Char 'w') v (x,y) | v < 0
mkMove (Char 'a') v (x,y)
mkMove (Char 's') v (x,y)
mkMove (Char 'd') v (x,y) -}


acceleration :: Velocity -> Velocity -> Velocity
acceleration vel newVel | newVel < 0 && (vel + newVel) >= 0   = vel + newVel
                        | newVel < 0 && vel + newVel < 0      = 0
                        | vel + newVel >= 20                  = 20
                        | otherwise                           = vel + newVel

handleKeys :: Event-> GameState -> GameState
handleKeys (EventKey key@(Char c) state _ _) g@(MkGameState ks s (MkPlayer n gs (x,y) vel l o) e a d st p)
  | c == 'w' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) (acceleration vel 2) l o) e a d st p
                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o) e a d st p
  | c == 'a' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) (acceleration vel 2) l o) e a d st p
                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o) e a d st p
  | c == 'd' =  case state of Down  -> MkGameState (insert key ks) s (MkPlayer n gs (x,y) (acceleration vel 2) l o) e a d st p
                              Up    -> MkGameState (delete key ks) s (MkPlayer n gs (x,y) vel l o) e a d st p
handleKeys _ game = game
