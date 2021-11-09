module Input where
-- INPUT --
-- For impure functions.

import Struct
import Menu
import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import System.Random
import System.Directory
import System.Exit
import Control.Monad

-- Takes the quitting gamestate and returns a safe exit.
quitScreen :: GameState -> IO Picture
quitScreen (MkQuitGame) = do exitWith (ExitSuccess)

middle x = x / 2

-- Mostly pure render function that also requires impurity.
-- getScreenSize is an impure function and is required to avoid resolution issues.
gameScreen :: GameState -> IO Picture
gameScreen (MkGameState ks c (MkPlayer _ gs im _ _ 0 o oo) en b _ hd _ )
  = do (x',y') <- getScreenSize
       let (x,y) = ( fromIntegral x', fromIntegral y' )
       let (width, height) = (middle x, middle y)
       let (left, right) = ( negate width, width )
       let (top, bottom) = ( height, negate height )
       pure (pictures [deadText, makeText ("Score: " ++ show gs) (-260) (-40) 0.5])
gameScreen (MkGameState ks c (MkPlayer _ gs im (x,y) _ l o oo) en b _ hd _ )
  = do (x'',y'') <- getScreenSize
       let (x',y') = ( fromIntegral x'', fromIntegral y'' )
       let (width, height) = (middle x', middle y')
       let (left, right) = ( negate width, width )
       let (top, bottom) = ( height, negate height )
       let gsText = makeText ("Score: " ++ show gs) (left+160) (top-80) 0.5
       let lifeText = makeText ("Lives: " ++ show l) (left+160) (top-160) 0.5
       if fst hd then
          if c `mod` 5 == 0 then do let player = mkPlayer im black x y o:(enemies++bullets)
                                    (pure . pictures) (player ++ [gsText, lifeText])
                            else do let player = mkPlayer im white x y o:(enemies++bullets)
                                    (pure . pictures) (player ++ [gsText, lifeText])
       else do let player = mkPlayer im white x y o:(enemies++bullets)
               (pure . pictures) (player ++ [gsText, lifeText])

    where enemies = map (createPicture white) en
          eb = [b | (Spaceship _ _ _ b) <- en]
          bullets = map (\(MkBullet _ (newX, newY) o _) ->
                    translate newX newY $ rotate (360-o) $ color white $ ThickCircle 1.5 3) (b++eb)

-- Writes a new version of highscores to file.
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

-- Reads the current highscores file and turns it to an IO Picture.
getHistory :: GameState -> IO Picture
getHistory m@(MkHighScore _ name score _ _)
  = do let filepath = "highscores.txt"
       fileExist <- doesFileExist filepath
       if not fileExist then writeFile filepath "" else pure ()
       content <- readFile filepath
       () <- pure (foldr seq () content)
       (x',y') <- getScreenSize
       let (x,y) = ( fromIntegral x', fromIntegral y' )
       let (width, height) = (middle x, middle y)
       let (left, right) = ( negate width, width )
       let (top, bottom) = ( height, negate height )
       let contents = lines content
       pure (pictures [ paintPicture contents (-80) (height-140) emptyPic,
                        makeText "Press Enter to go back" (-240) (bottom+140) 0.3])

-- updateScore --
-- Gives highscores with new score if conditions are met, else gives back old highscores.
-- true if:
-- (1) less than 10 spots occupied
-- (2) if 10 spots but one has lesser score
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
-- pos = 1

updateScore :: String -> Int -> [String] -> String
updateScore name score [] = name ++ ": " ++ show score ++ "\n"
updateScore name score contents
  | length lesserIdx > 0 = newContents
  | length contents < 10 = unlines (contents ++ [newScore])
  | otherwise = unlines contents
    where newScore = name ++ ": " ++ show score
          lesserIdx = [idx | (idx,x) <- zip [0..] contents, let x' = words x,
                       let oldScore = (read . head . tail) x', score > oldScore]
          pos = head lesserIdx
          newContents = (unlines . take 10) (take pos contents ++ [newScore] ++ drop pos contents)


