module Controller.App where

import Control.Monad
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
      ButtonLeft -> alter $ onMouseMotion (fromIntegral x) (fromIntegral y) >=> updateModel >=> updateModelView
      _          -> loopback app
    _ -> loopback app
 where quit     = return (False,app)
       nochange = return (True,app)
       loopback = handleEvents
       alter f  = f app >>= loopback

onMouseMotion :: Double -> Double -> AppController -> IO AppController
onMouseMotion x y app =
  let w          = appWidth app
      h          = appHeight app
      cz         = appZoom app
      czf        = appZoomFactor app
      (rx :+ ry) = toCart w h (x :+ y)
      (nx,ny)    = (appX app + rx/cz,appY app + ry/cz)
      newZ       = cz * czf
  in return app { appX = nx, appY = ny, appZoom = newZ }

updateModel :: AppController -> IO AppController
updateModel app = do
  let p = appProgression app
      m = appModel app
      w = appWidth app
      h = appHeight app
      x = appX app
      y = appY app
      z = appZoom app
      i = appMaxIter app
  putStr "updating fractal... "
  newModel <- regen p m w h x y z i
  putStrLn "done!"
  return app { appModel = newModel }

updateModelView :: AppController -> IO AppController
updateModelView app = do
  pixelizeSurface (appModel app) $ stdViewFractalSurface $ appFView app
  return app
