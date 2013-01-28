module UI where

import Application
import Graphics.UI.SDL (waitEvent)

treatEvents :: App -> IO (Bool,Application)
treatEvents app = 
  event <- waitEvent
  case event of
    NoEvent -> nochange
    Quit    -> quit
    KeyUp k -> case symKey k of
      SDLK_ESCAPE -> quit
      SDLK_c      -> alter $ onViewerUpdate ModifColorSeed)
      SDLK_s      -> alter $ onScreenshot
      _           -> loopback
    MouseButtonUp x y b -> case b of
      ButtonLeft -> alter $ onIterFrameUpdate x y
    _       -> loopback
 where quit = return (False,app)
       nochange = return (True,app)
       alter f = return (True,f app)
       loopback = treatEvents app
