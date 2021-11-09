{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Struct where
-- Game architecture including data types, aliases and constants.

import Data.List
import Graphics.Gloss
import Data.Number.CReal
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import qualified GHC.Base as S
import System.Random
import GJK.Collision
import GJK.Support

-- All the different gamestates.
data GameState
  = MkEnterName
    { textBoxes :: [Picture],
      name :: String }
  | MkMainMenu -- TBD: encapsulate EnterName
    { textBoxes :: [Picture],
      name :: String,
      score :: Int,
      finished :: Bool }
  | MkHighScore 
    { textBoxes :: [Picture],
      name :: String,
      score :: Int,
      inGame :: Bool,
      game :: GameState }
  | MkGameState 
    { keys :: [Key], 
      counter :: Int,
      player :: Player, 
      enemies :: [Enemy], 
      allArtilery :: [Bullet],
      difficulty :: Difficulty,
      hasDied :: (Bool,Int),
      sg :: StdGen } 
  | MkPauseMenu
    { game :: GameState,
      textBoxes :: [Picture] }
  | MkGameOver
    { name :: String,
      score :: Int,
      counter :: Int,
      written :: Bool }
  | MkQuitGame

-- The user-controlled player.
data Player = MkPlayer { 
  pName :: Name,
  gameScore :: GameScore,
  isMoving :: Bool,
  position :: Position,
  velocity :: Velocity,
  lives :: Lives,
  orientation :: Orientation,
  oldOrientation :: Orientation
}

-- Enemies that may damage the player.
data Enemy
  = Asteroid Size Position Orientation
  | Spaceship Size Position Orientation Bullet

type Position = (Float, Float)
type Name  = String
type GameScore = Int
type Orientation = Float
type Velocity = Float
type Lives = Int

data Difficulty = Hard | Medium | Easy
data Size = Large | Med | Small deriving(Eq)
data Bullet = MkBullet Position Position Orientation Velocity

window :: Display
window = FullScreen

background :: Color
background = black

fps :: Int
fps = 60
