module Menu where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Struct

enterBox = [enterText, enterBorder]
menuBox = [startText, startBorder, scoreText, scoreBorder, quitText, quitBorder]
pauseBox = [resumeText, resumeBorder, scoreText, scoreBorder, quitText, quitBorder]

makeText :: String -> Float -> Float -> Picture
makeText name x y
  = Translate x y
  $ Scale 0.5 0.5
  $ Color white
  $ Text name

makeScore :: String -> Int -> Picture
makeScore name score 
  = Translate (-240) (40) -- TBD
  $ Scale 0.5 0.5
  $ Color white
  $ Text (show (name ++ ": " ++ (show score)))

enterText
  = Translate (-240) (40)
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Enter your name"

enterBorder
  = Translate (20) (-20)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (132)

-- data TextBox

startText
  = Translate (-160) (-40)
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Start game"

startBorder
  = Translate (20) (-20)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (132)

scoreText
  = Translate (-166) (-120)
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "High scores"

scoreBorder
  = Translate (20) (-100)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (132) 

resumeText
  = Translate (-210) (-40)
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Resume game"

resumeBorder
  = Translate (20) (-20)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (132)

quitText
  = Translate (-150) (-200)
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Quit game"

quitBorder
  = Translate (20) (-180)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (132) 

deadText
  = Translate (-240) (40)
  $ Scale 0.8 0.8
  $ Color white
  $ rectangleWire (1600) (132)
