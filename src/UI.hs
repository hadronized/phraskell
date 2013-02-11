module UI where

import Application
import Control.Monad
import Graphics.UI.SDL
import GUI
import UI.Impl
import Viewer

treatEvents :: App -> IO (Bool,App)
treatEvents app = do
  event <- pollEvent
  case event of
    NoEvent -> nochange
    Quit    -> quit
    KeyUp k -> case symKey k of
      SDLK_ESCAPE    -> quit
      SDLK_SPACE     -> queryCursorState >>= showCursor . not >> nochange
      SDLK_MINUS     -> alter $ (\a -> let v    = appViewer a
                                           maxi = viewerMaxIter v
                                       in return a { appViewer = v { viewerMaxIter = maxi-50 } })
      SDLK_PLUS      -> alter $ (\a -> let v    = appViewer a
                                           maxi = viewerMaxIter v
                                       in return a { appViewer = v { viewerMaxIter = maxi+50 } })
      _           -> loopback app
    KeyDown k -> case symKey k of
      SDLK_LEFTPAREN  -> do
        let v  = appViewer app
            zf = viewerZoomf v
            na = app { appViewer = v { viewerZoomf = zf-0.1 } }
        newGUI <- updateGUIZoomArea (appGUI na) (appViewer na)
        loopback $ na { appGUI = newGUI }
      SDLK_RIGHTPAREN  -> do
        let v  = appViewer app
            zf = viewerZoomf v
            na = app { appViewer = v { viewerZoomf = zf+0.1 } }
        newGUI <- updateGUIZoomArea (appGUI na) (appViewer na)
        loopback $ na { appGUI = newGUI }

      _               -> loopback app
    MouseButtonUp x y b -> case b of
      ButtonLeft -> alter $ onIterFrameUpdate (fromIntegral x) (fromIntegral y)
      _ -> loopback app
    _  -> loopback app
 where quit     = return (False,app)
       nochange = return (True,app)
       alter f  = f app >>= loopback
       loopback = treatEvents
