import Graphics.Gloss

main 
  = display (FullScreen) 
    black
    picture

picture 
  = Translate (-170) (-20) 
  $ Scale 0.5 0.5 
  $ Color white
  $ Text "Enter your name"
