module Controller.App where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Controller.Bootstrap
import Controller.CLI
import Controller.Fractal
import Controller.GUI
import Data.Maybe (maybe)
import Graphics.UI.SDL as SDL
import Model.Fractal
import Model.Progression
import View.Fractal

data AppController = AppController {
    appScreen      :: Surface
  , appFractalCtrl :: FractalController
  , appGUICtrl     :: GUIController
}

runCtrl :: AppController -> IO ()
runCtrl app = do
  showCursor False
  enableKeyRepeat 200 10
  -- TODO: make the first render
  loop app
  SDL.quit

loop :: AppController -> IO ()
loop app = do
  (continue,newApp) <- handleEvents app
  when continue $ do
    runFractalCtrl (appFractalCtrl app)
    runGUICtrl (appGUICtrl app)
    SDL.flip $ appScreen app
    loop app

handleEvents :: AppController -> IO (Bool,AppController)
handleEvents app = do
  event <- pollEvent
  case event of
    NoEvent -> nochange
    Quit    -> quit
    KeyUp k -> case symKey k of
      SDLK_ESCAPE -> quit
      SDLK_SPACE  -> loopback $ app { appGUICtrl = toggleGUI $ appGUICtrl app }
      SDLK_RETURN -> loopback app
      SDLK_MINUS  -> loopback app
      SDLK_PLUS   -> loopback app
      _           -> loopback app
    KeyDown k -> case symKey k of
      SDLK_LEFTPAREN  -> loopback app
      SDLK_RIGHTPAREN -> loopback app
      _               -> loopback app
    MouseButtonUp x y b -> case b of
      ButtonLeft -> loopback $ let gui = appGUICtl app
                                   w   = gui
                               in app { appFractalCtrl = regenModel (appFractalCtrl app)  }
      _          -> loopback app
    _ -> loopback app
 where quit     = return (False,app)
       nochange = return (True,app)
       loopback = handleEvents
