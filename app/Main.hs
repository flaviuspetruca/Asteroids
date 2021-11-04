module Main where
-- MAIN --
-- For handling IO impurity as well as running the game.

import Lib
import Struct
import Menu
import Logic
import View
import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Directory
import Control.Monad

main = playIO window background fps initState handleRender handleEvent handleUpdate

initState = MkEnterName enterBox ""

handleRender :: GameState -> IO Picture
handleRender hs@(MkHighScore _ _ _ _ _) = do getHistory hs
handleRender g = pure (render g)

handleEvent :: Event -> GameState -> IO GameState
handleEvent event g
  = do let event' = handleKeys event g
       pure event'

handleUpdate :: Float -> GameState -> IO GameState
handleUpdate sec mm@(MkMainMenu _ name score True)
  = do writeHistory mm
       let mm' = update sec mm
       pure mm'
handleUpdate sec m = pure (update sec m)

writeHistory :: GameState -> IO ()
writeHistory (MkMainMenu _ name score True)
  = do let filepath = "highscores.txt"
       fileExist <- doesFileExist filepath
       if not fileExist then writeFile filepath "" else pure ()
       content <- readFile filepath
       () <- pure (foldr seq () content)
       let contents = lines content
       let highscore = updateScore name score contents
       writeFile filepath highscore

getHistory :: GameState -> IO Picture
getHistory m@(MkHighScore _ name score _ _)
  = do let filepath = "highscores.txt"
       fileExist <- doesFileExist filepath
       if not fileExist then writeFile filepath "" else pure ()
       content <- readFile filepath
       () <- pure (foldr seq () content)
       let contents = lines content
       history <- pure (paintPicture contents (-200) (400) emptyPic)
       pure history

-- true if:
-- (1) less than 10 spots occupied
-- (2) if 10 spots but one has lesser score
--
-- contents = ["martin: 356","marty: 949"]
-- content = "martin: 356"
-- content' = ["martin:", "356"]
--
-- when true:
-- (1) insert new score
-- (2) push rest of scores one notch down
--
-- example:
-- 1. marty: 394
-- 2. troy: 93
-- 3. axel: 90
-- ...
-- 10. bob: 30
--
-- newScore = egil: 150

updateScore :: String -> Int -> [String] -> String
updateScore name score [] = name ++ ": " ++ (show score) ++ "\n"
updateScore name score contents
  | length lesserIdx > 0 = newContents
  | length contents < 10 = concat (contents' ++ [newScore])--(reverse (newScore : (reverse contents')))
  | otherwise = concat contents'
    where newScore = name ++ ": " ++ (show score) ++ "\n"
          lesserIdx = [idx | (idx,x) <- zip [0..] contents, let x' = words x,
                       let oldScore = (read . head . tail) x', score > oldScore]
          pos = head lesserIdx
          contents' = map (++ "\n") contents
          newContents = (concat . take 10) (take pos contents' ++ [newScore] ++ drop pos contents')--(take 10 (take pos contents' ++ [newScore] ++ drop pos contents'))


