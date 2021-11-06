{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Struct where

import Data.List
import Graphics.Gloss
import Data.Number.CReal
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import qualified GHC.Base as S
import System.Random
import GJK.Collision
import GJK.Support

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
      started :: Bool,
      paused :: Bool,
      sg :: StdGen } 
  | MkPauseMenu
    { game :: GameState,
      textBoxes :: [Picture] }
  | MkGameOver
    { name :: String,
      score :: Int,
      counter :: Int }
  | MkQuitGame

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

data Movement = MkMovement {
  pos :: Position,
  orient :: Orientation,
  vel :: Velocity
}

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

width, height, offset :: Int
width = 900
height = 600
offset = 50

fps :: Int
fps = 60

instance Show GameState where
  show (MkGameState ks c (MkPlayer n gs im pos vel l o oo) enemies a d hd st p sg) = show sg

instance Show Enemy where
  show (Asteroid s p o) = show p ++ show o
  show Spaceship {} = show 2

