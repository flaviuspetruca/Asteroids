module Menu where
-- MENU --
-- For generating menu functionality.

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment

import Struct

emptyPic :: Picture
emptyPic = makeText "" 0 0 0.5

--returnPic :: Picture
--returnPic = makeText "Press Enter to go back" (-400) (-400) 0.3

enterBox = [enterText, enterBorder]
menuBox = [startText, startBorder, scoreText, scoreBorder, quitText, quitBorder]
pauseBox = [resumeText, resumeBorder, scoreText, scoreBorder, quitText, quitBorder]

-- Potential foldr/foldl application?
paintPicture :: [String] -> Float -> Float -> Picture -> Picture
paintPicture [] x y pic = pic--pictures [pic, (makeText "Press Enter to go back" (x-200) (y-80) 0.3)]
paintPicture (name:names) x startY pic
  = pictures [pic, (makeText name x startY 0.3), (paintPicture names x nextY pic)]
    where nextY = (startY - 40)

--paintPicture :: [String] -> Float -> Float -> Picture -> Picture
--paintPicture names x startY pic = foldr (\name acc -> let y = (y - 80) in (pictures [pic, (makeText name x y), acc])) pic names
--  where y = startY

makeText :: String -> Float -> Float -> Float -> Picture
makeText name x y scale
  = Translate x y
  $ Scale scale scale
  $ Color white
  $ Text name

makeScore :: String -> Int -> Picture
makeScore name score 
  = Translate (-240) (40)
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
  = Translate (-260) (40)
  $ Scale 0.8 0.8
  $ Color white
  $ Text "Game over"

replayBorder
  = Translate (20) (-20)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (132)

backBorder
  = Translate (20) (-100)
  $ Scale 0.6 0.6
  $ Color blue
  $ rectangleWire (1600) (132)
