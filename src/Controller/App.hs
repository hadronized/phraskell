module Controller.App where

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
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
  --showCursor False
  enableKeyRepeat 200 10
  -- TODO: make the first render
  loop app
  SDL.quit

loop :: AppController -> IO ()
loop app = do
  (continue,newApp) <- handleEvents app
  when continue $ do
    -- TODO: here
    F.expose (appModel newApp) (appFView newApp)
    (mx,my,_) <- getMouseState
    exposeGUI mx my app
    SDL.flip $ appScreen newApp
    loop newApp

handleEvents :: AppController -> IO (Bool,AppController)
handleEvents app = do
  event <- pollEvent
  case event of
    NoEvent -> nochange
    Quit    -> quit
    KeyUp k -> case symKey k of
      SDLK_ESCAPE -> quit
      SDLK_SPACE  -> loopback $ app { appGUIVisible = toggle (appGUIVisible app) }
      SDLK_RETURN -> alter $ updateModel >=> updateModelView
      SDLK_MINUS  -> loopback $ changeMaxIter (50-) app
      SDLK_PLUS   -> loopback $ changeMaxIter (50+) app
      _           -> loopback app
    KeyDown k -> case symKey k of
      SDLK_LEFTPAREN  -> alter $ return . changeZoomWindowSize (/1.05) >=> updateGUIZoomWindow
      SDLK_RIGHTPAREN -> alter $ return . changeZoomWindowSize (*1.05) >=> updateGUIZoomWindow
      _               -> loopback app
    MouseButtonUp x y b -> case b of
      ButtonLeft -> alter $ (onMouseButtonLeft (fromIntegral x) (fromIntegral y) >=> updateModel >=> updateModelView)
      _          -> loopback app
    _ -> loopback app
 where quit     = return (False,app)
       nochange = return (True,app)
       loopback = handleEvents
       alter f  = f app >>= loopback

onMouseButtonLeft :: Double -> Double -> AppController -> IO AppController
onMouseButtonLeft x y app =
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
  putStr $ "updating fractal [x:" ++ show x ++ " y:" ++ show y ++ " z:" ++ show z ++ " i:" ++ show i ++ "] ..."
  newModel <- regen p m w h x y z i
  putStrLn "done!"
  return app { appModel = newModel }

updateModelView :: AppController -> IO AppController
updateModelView app = do
  pixelizeSurface (appModel app) $ stdViewFractalSurface $ appFView app
  return app

exposeGUI :: Int -> Int -> AppController -> IO AppController
exposeGUI x y app = do
  let rw = floor $ appWidth app / zf
      rh = floor $ appHeight app / zf
      rx = x - rw `div` 2
      ry = y - rh `div` 2
      zf = appZoomFactor app
      gv = appGView app
  case gv of
    G.StandardView _ za -> do
      blitSurface za Nothing (appScreen app) (Just $ Rect rx ry rw rh)
      return app

changeZoomWindowSize :: (Double -> Double) -> AppController -> AppController
changeZoomWindowSize f app = let zf = appZoomFactor app in app { appZoomFactor = f zf }

updateGUIZoomWindow :: AppController -> IO AppController
updateGUIZoomWindow app = do
  let w  = appWidth app
      h  = appHeight app
      zf = appZoomFactor app
  newZoomWindow <- runMaybeT $ tryCreateZoomWindow (floor w) (floor h) zf
  case newZoomWindow of
    Nothing -> return app
    Just zoomWindow  -> do
      let gv  = appGView app
      case gv of
        G.StandardView _ zw -> do
          let ngv = gv { stdViewZoomArea = zoomWindow }
          return app { appGView = ngv }

changeMaxIter :: (Integer -> Integer) -> AppController -> AppController
changeMaxIter f app = let mi = appMaxIter app in app { appMaxIter = f mi }
