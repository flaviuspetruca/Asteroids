module Window where

import Graphics.Gloss

playy
  = animate (FullScreen) -- should be simulate or play
    black
    (\x -> picture)

picture 
  = Translate (-170) (-20) 
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Enter your name"
