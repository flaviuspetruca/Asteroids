module Menu where

import Graphics.Gloss

name = [nameText, nameBox]
menu = [startText, startBox, scoreText, scoreBox]

nameText
  = Translate (-20) (-40)
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Enter your name"

nameBox
  = Translate (220) (-20)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (120)

-- data TextBox

startText
  = Translate (-20) (-80)
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Start game"

startBox
  = Translate (220) (-60)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (120)

scoreText
  = Translate (-20) (-180)
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Highscores"

scoreBox
  = Translate (220) (-160)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (120)
