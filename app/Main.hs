module Main where
-- MAIN --
-- For handling IO impurity as well as running the game.
-- See Input.hs for impure helper functions.

import Struct
import Input
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

-- IO wrappers for various gamestates.
handleRender :: GameState -> IO Picture
handleRender hs@(MkHighScore _ _ _ _ _) = do getHistory hs
handleRender q@(MkQuitGame) = do quitScreen q
handleRender g@(MkGameState _ _ _ _ _ _ _ _) = do gameScreen g
handleRender g = do let g' = render g
                    pure g'

-- IO wrapper for input keys - for consistency.
handleEvent :: Event -> GameState -> IO GameState
handleEvent event g
  = do let event' = handleKeys event g
       pure event'

-- IO wrapper for update function.
-- When returning from a finished game,
-- it always updates the highscores.
handleUpdate :: Float -> GameState -> IO GameState
handleUpdate sec mm@(MkMainMenu _ name score True)
  = do writeHistory mm
       let mm' = update sec mm
       pure mm'
handleUpdate sec m = pure (update sec m)
