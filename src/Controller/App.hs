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
import View.Fractal as F
import View.GUI as G

data AppController = AppController {
    appScreen      :: Surface
  , appProgression :: FractalProgression
  , appWidth       :: Double
  , appHeight      :: Double
  , appX           :: Double
  , appY           :: Double
  , appZoom        :: Double
  , appZoomFactor  :: Double
  , appMaxIter     :: Integer
  , appModel       :: FractalModel
  , appFView       :: FractalView
  , appGUIVisible  :: Bool
  , appGView       :: GUIView
}

run :: AppController -> IO ()
run app = do
  showCursor False
  enableKeyRepeat 200 10
  -- TODO: make the first render
  loop app
  SDL.quit

loop :: AppController -> IO ()
loop app = do
  (continue,newApp) <- handleEvents app
  when continue $ do
    -- TODO: here
    F.expose (appModel app) (appFView app)
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
      SDLK_SPACE  -> loopback $ app { appGUIVisible = toggle (appGUIVisible app) }
      SDLK_RETURN -> loopback app
      SDLK_MINUS  -> loopback app
      SDLK_PLUS   -> loopback app
      _           -> loopback app
    KeyDown k -> case symKey k of
      SDLK_LEFTPAREN  -> loopback app
      SDLK_RIGHTPAREN -> loopback app
      _               -> loopback app
    MouseButtonUp x y b -> case b of
      ButtonLeft -> do
        putStr "updating fractal... "
        let w          = appWidth app
            h          = appHeight app
            cz         = appZoom app
            czf        = appZoomFactor app
            (rx :+ ry) = toCart w h (fromIntegral x :+ fromIntegral y)
            (nx,ny)    = (appX app + rx/cz,appY app + ry/cz)
            newZ       = cz * czf
        newModel <- updateModel (appProgression app) (appModel app) w h nx ny cz (appMaxIter app)
        pixelizeSurface newModel $ stdViewFractalSurface $ appFView app
        putStrLn "done!"
        loopback $ app { appModel = newModel, appX = nx, appY = ny, appZoom = newZ }
      _          -> loopback app
    _ -> loopback app
 where quit     = return (False,app)
       nochange = return (True,app)
       loopback = handleEvents
