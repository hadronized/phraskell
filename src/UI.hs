module UI where

import Application
import Graphics.UI.SDL
import UI.Impl

treatEvents :: App -> IO (Bool,App)
treatEvents app = do
  event <- waitEvent
  case event of
    NoEvent -> nochange
    Quit    -> quit
    KeyUp k -> case symKey k of
      SDLK_ESCAPE -> quit
      _           -> loopback
    MouseButtonUp x y b -> case b of
      ButtonLeft -> alter $ onIterFrameUpdate (fromIntegral x) (fromIntegral y) 0.5
    _       -> loopback
 where quit     = return (False,app)
       nochange = return (True,app)
       alter f  = f app >>= \a -> return (True,a)
       loopback = treatEvents app
