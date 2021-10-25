module Window(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
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
    score = 0,
    player = MkPlayer{ 
      name = "Flavius",
      gameScore = 0,
      position = (0,0),
      velocity = 0.0,
      lives = 3,
      orientation = 90
    },
    enemies = [Asteroid, Asteroid, Asteroid],
    allArtilery = [(1,1,1), (1,2,2)],
    difficulty = Easy,
    started = True, --should be false(going to change it after we create the menu)
    paused = False
  }

mkPlayer :: Color -> Float -> Float -> Picture 
mkPlayer c x y = translate x y $ color c $ Polygon [(15, 0), (-15,10),(-10,0),(-15,-10),(15,0)]

-- | Convert a game state into a picture.
render :: GameState -> Picture
render (MkGameState _ (MkPlayer _ _ (x,y) _ _ _) _ _ _ _ _) = pictures [mkPlayer white x y]

-- | Update the spaceship movement using its current velocity.
movePlayer :: Float    -- ^ The number of seconds since last update
         -> GameState -- ^ The initial game state
         -> GameState -- ^ A new game state with an updated spaceship movement
movePlayer seconds (MkGameState s (MkPlayer _ _ (x,y) vel _ _) _ _ _ _ _) = newGame
  where newGame = MkGameState s (MkPlayer "Flavius" 0 (x,y) vel 3 90) [Asteroid] [] Easy True False 


update ::  Float -> GameState -> GameState
update sec (MkGameState s (MkPlayer n gs (x,y) vel l o) e a d st p) = movePlayer sec (MkGameState s (MkPlayer n gs (x,y) (acceleration vel (-0.5)) l o) e a d st p)

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

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 's') _ _ _) (MkGameState s (MkPlayer n gs (x,y) vel l o) e a d st p)  = MkGameState s (MkPlayer n gs (x,y-vel) (acceleration vel 2) l o) e a d st p
handleKeys (EventKey (Char 'w') _ _ _) (MkGameState s (MkPlayer n gs (x,y) vel l o) e a d st p)  = MkGameState s (MkPlayer n gs (x,y+vel) (acceleration vel 2) l o) e a d st p
handleKeys (EventKey (Char 'a') _ _ _) (MkGameState s (MkPlayer n gs (x,y) vel l o) e a d st p)  = MkGameState s (MkPlayer n gs (x-vel,y) (acceleration vel 2) l o) e a d st p
handleKeys (EventKey (Char 'd') _ _ _) (MkGameState s (MkPlayer n gs (x,y) vel l o) e a d st p)  = MkGameState s (MkPlayer n gs (x+vel,y) (acceleration vel 2) l o) e a d st p
handleKeys _ game = game
