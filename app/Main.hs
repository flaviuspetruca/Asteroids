module Main where

import Lib
import Struct
import Menu
import Logic
import Window
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
--import Graphics.Gloss.Interface.IO.Game
--    ( Key(Char, SpecialKey, MouseButton),
--      KeyState(Up, Down),
--      SpecialKey(KeySpace, KeyEnter, KeyBackspace),
--      Event(EventKey),
--      MouseButton (LeftButton) )
import System.Random
import Control.Monad

main = playIO window background fps initState handleRender handleEvent handleUpdate
--main = playIO window background fps initialMenuState renderIO 

handleRender :: GameState -> IO Picture
handleRender hs@(MkHighScore _ _ _ _ _) = do getHistory hs
handleRender g = pure (render g)

handleEvent :: Event -> GameState -> IO GameState
handleEvent event g
  = do let event' = handleKeys event g
       pure event'

handleUpdate :: Float -> GameState -> IO GameState
handleUpdate sec g@(MkGameState _ (MkPlayer _ _ _ _ _ 0 _ _) _ _ _ _ _ _)
  = do writeHistory g
       let g' = update sec g
       pure g'
handleUpdate sec m = pure (update sec m)

writeHistory :: GameState -> IO ()
writeHistory g@(MkGameState _ (MkPlayer name score _ _ _ 0 _ _) _ _ _ _ _ _)
  = do let filepath = "/home/martin/indie/uni/fp/Asteroids/highscores.txt"
       putStrLn "is this invoked multiple times?"
       highscore <- compareToFile (name ++ ": ") score filepath
       when highscore $ do appendFile filepath ((name ++ ": ") ++ (show score) ++ ("\n"))

getHistory :: GameState -> IO Picture
getHistory m@(MkHighScore _ name score _ _)
  = do let filepath = "/home/martin/indie/uni/fp/Asteroids/highscores.txt"
       content <- readFile filepath
       () <- pure (foldr seq () content)
       history <- pure (makeText content (240) (40))
       pure history

compareToFile :: String -> Int -> String -> IO Bool
compareToFile name score filepath
  = do content <- readFile filepath
       () <- pure (foldr seq () content)
       let contents = lines content
       --putStr content
       pure $ checkScore name score contents

checkScore :: String -> Int -> [String] -> Bool
checkScore name score [] = True
checkScore name score (content:contents)
  | length content == 0 = True -- "" case
  | score > oldScore = True
  | otherwise = checkScore name score contents
    where content' = words content
          oldName = head content'
          oldScore = (read . head . tail) content'


initState = MkEnterName enterBox ""

-- compareToFile :: String -> IO Bool
-- compareToFile s = checkScore s <$> readFile "my-file.txt"

-- handleEvent :: Event -> GameState -> IO GameState
-- handleEvent event state = do
-- let state' = handleKeys event state
-- makeFile state'
-- pure state'
--
-- makeFile :: GameState -> IO ()
--
-- main = playIO
--  mode
--  color
--  fps
--  initState
--  (pure . displayWorld)
--  handleEvent
--  (\dt world -> pure $ stepWrodl dt world)
