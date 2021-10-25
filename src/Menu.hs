module Menu where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Logic

--main = animate window background frame
--  where frame :: Float -> Picture
--        frame seconds = render $ enterName seconds menu initialState
--main = simulate window background fps initialState render update
main = play window background fps initialState render handleKeys update


window :: Display
window = FullScreen

background :: Color
background = black

enterBox = [enterText, enterBorder]
menuBox = [startText, startBorder, scoreText, scoreBorder]
pauseBox = [resumeText, resumeBorder, scoreText, scoreBorder, quitText, quitBorder]

initialState :: GameState
initialState = EnterName enterBox ""

fps :: Int
fps = 60

render :: GameState -> Picture
--render (MainMenu pics "")
--  = pictures pics
render (EnterName pics _) = pictures pics
render (MainMenu pics _ _) = pictures pics
render (GameState pics _ _ _ _ _) = pictures pics
render (HighScore pics _ _) = pictures pics

makeName :: String -> Float -> Float -> Picture
makeName name x y
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

-- TBD
enterName :: Float -> GameState -> GameState
enterName seconds game
  | seconds > 2.0 = game { textBoxes = menuBox }
  | otherwise = game

proceed :: GameState -> GameState
proceed game = game

update :: Float -> GameState -> GameState
--update _ = enterName
update seconds = proceed . enterName seconds

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyEnter) _ _ _) (EnterName boxes currName)
  | length currName > 0 = (MainMenu updatedBoxes currName 0)
  | otherwise = (EnterName boxes currName)
  where updatedBoxes = makeName currName (-460) (40) : menuBox
handleKeys (EventKey (SpecialKey KeyEnter) _ _ _) (HighScore boxes currName score)
  = (MainMenu updatedMenu currName score)
    where updatedMenu = makeName currName (-460) (40) : menuBox
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) (EnterName boxes currName)
  = (EnterName updatedBoxes updatedName)
  where updatedName
          | length currName > 0 = take (length currName - 1) currName
          | otherwise = currName
        updatedBoxes = makeName updatedName (namePos) (-40) : enterBox
        namePos = (-60) - fromIntegral (length (currName) * 15)
handleKeys (EventKey (Char ch) Down _ _) (EnterName boxes currName)
  = (EnterName updatedBoxes updatedName)
  where updatedName
          | length currName < 12 = currName ++ [ch]
          | otherwise = currName
        updatedBoxes = makeName updatedName (namePos) (-40) : enterBox
        namePos = (-60) - fromIntegral (length (currName) * 15)
handleKeys (EventKey (MouseButton LeftButton) Down _ mousePos) (MainMenu boxes currName score)
  | x > (-460) && x < 500 && y > (-60) && y < 20 = (GameState updatedPause currName 0 3 True (0,0) )
  | x > (-460) && x < 500 && y > (-142) && y < (-62) = (HighScore currScore currName score)
  | otherwise = (MainMenu boxes currName score)
    where updatedBoxes = makeName currName (-460) (80) : menuBox
          (x, y) = mousePos
          currScore = [(makeScore currName score), (makeName "Press Enter to go back" (-240) (-80))]
          updatedPause = makeName currName (-460) (40) : pauseBox
handleKeys (EventKey (MouseButton LeftButton) Down _ mousePos) (GameState box name score hp True pos)
  | x > (-460) && x < 500 && y > (-224) && y < (-144) = (MainMenu box name score)
  | otherwise = (GameState box name score hp True pos)
    where (x, y) = mousePos
handleKeys _ game = game

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
