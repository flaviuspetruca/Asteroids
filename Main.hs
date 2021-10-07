module Main where
  
import Data.List (intercalate)
main :: IO ()
main = print("hello")
data Position = Pt Float Float
type Name  = String
type GameScore = Int
type Orientation = Float
type Speed = Float
type Lives = Int
data Difficulty = Hard | Medium | Low
type Bullet = (Float, Float, Float)

class Movable a where move :: a -> a


data Player = MkPlayer {
  name ::Name,
  gameScore :: GameScore,
  position :: Position,
  lives :: Lives,
  orientation :: Orientation
}

data Movement = MkMovement {
  pos :: Position, 
  orient :: Orientation, 
  speed :: Speed
}


data Enemy =  
  Asteroid { movement:: Movement } |
  Spaceship {
    movement :: Movement, 
    artillery :: [Bullet]
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

data MainMenu = Menu {
  playerName :: Name, 
  highScores :: [(Name, Int)], 
  gameState :: GameState
}

