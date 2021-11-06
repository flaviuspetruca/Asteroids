module Input where
-- INPUT --
-- For impure functions.

import Lib
import Struct
import Menu
import Logic
import View
import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import System.Random
import System.Directory
import Control.Monad

gameScreen :: GameState -> IO Picture
gameScreen (MkGameState ks c (MkPlayer _ gs im _ _ 0 o oo) en b _ hd _ _ _ )
  = do (x',y') <- getScreenSize
       let (x,y) = ( (fromIntegral x'), (fromIntegral y') )
       let (width, height) = ((middle x), (middle y))
       let (left, right) = ( (0 - width), width )
       let (top, bottom) = ( height, (0 - height) )
       pure (pictures [deadText, makeText ("Score: " ++ show gs) (-260) (-40)])
gameScreen (MkGameState ks c (MkPlayer _ gs im (x,y) _ l o oo) en b _ hd _ _ _ )
  = do (x'',y'') <- getScreenSize
       let (x',y') = ( (fromIntegral x''), (fromIntegral y'') )
       let (width, height) = ((middle x'), (middle y'))
       let (left, right) = ( (0 - width), width )
       let (top, bottom) = ( height, (0 - height) )
       let gsText = makeText ("Score: " ++ show gs) (left+160) (top-80)
       let lifeText = makeText ("Lives: " ++ show l) (left+160) (top-160)
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
                    translate newX newY $ rotate (360-o) $ color white $ ThickCircle 1.5 3) (b++eb)

addStatus :: GameState -> IO Picture
addStatus (MkGameState ks c (MkPlayer _ gs im _ _ l o oo) en b _ hd _ _ _ )
  = do (x',y') <- getScreenSize
       let (x,y) = ( (fromIntegral x'), (fromIntegral y') )
       let (width, height) = ((middle x), (middle y))
       let (left, right) = ( (0 - width), width )
       let (top, bottom) = ( height, (0 - height) )
       let gsText = makeText (show gs) (left+160) (top-80)
       let lifeText = makeText (show l) (left+160) (top-160)
       pure (pictures [gsText, lifeText])

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
       (x',y') <- getScreenSize
       let (x,y) = ( (fromIntegral x'), (fromIntegral y') )
       let (width, height) = ((middle x), (middle y))
       let contents = lines content
       history <- pure (paintPicture contents (-200) (height-140) emptyPic)
       pure history

-- true if:
-- (1) less than 10 spots occupied
-- (2) if 10 spots but one has lesser score
--
-- contents = ["martin: 356","marty: 949"]
-- content = "martin: 356"
-- content' = ["martin:", "356"]
-- unlines contents = "martin: 356\nBob: 99"
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
updateScore name score [] = name ++ ": " ++ (show score) ++ "\n"
updateScore name score contents
  | length lesserIdx > 0 = newContents
  | length contents < 10 = unlines (contents ++ [newScore])
  | otherwise = unlines contents
    where newScore = name ++ ": " ++ (show score)
          lesserIdx = [idx | (idx,x) <- zip [0..] contents, let x' = words x,
                       let oldScore = (read . head . tail) x', score > oldScore]
          pos = head lesserIdx
          newContents = (unlines . take 10) (take pos contents ++ [newScore] ++ drop pos contents)


