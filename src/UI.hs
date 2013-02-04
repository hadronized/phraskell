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
      SDLK_SPACE  -> queryCursorState >>= showCursor . not >> nochange
      _           -> loopback
    MouseButtonUp x y b -> case b of
      ButtonLeft -> alter $ onIterFrameUpdate (fromIntegral x) (fromIntegral y) 2
    MouseMotion x y _ _ -> alter $ onMouseMotion (fromIntegral x) (fromIntegral y) 2
    _  -> loopback
 where quit     = return (False,app)
       nochange = return (True,app)
       alter f  = f app >>= \a -> return (True,a)
       loopback = treatEvents app
