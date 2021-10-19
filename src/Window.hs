module Window where

import Graphics.Gloss

playy
  = simulate (FullScreen) -- should be simulate or play
    black

picture 
  = Translate (-170) (-20) 
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Enter your name"
