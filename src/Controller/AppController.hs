module Controller.AppController where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Controller.Bootstrap
import Controller.CLI
import Controller.FractalController
import Controller.GUIController
import Data.Maybe (maybe)
import Graphics.UI.SDL as SDL
import Model.Fractal
import Model.Progression
import View.FractalView

data AppController = AppController {
    appScreen   :: Surface
  , fractalCtrl :: FractalController
  , guiCtrl     :: GUIController
}

fromBootstrap :: Bootstrap -> IO (Maybe AppController)
fromBootstrap b = do
  screen <- tryGetScreen $ (bootWidth b) (bootHeight b) 32 "phraskell"
  case screen of
    Nothing  -> return Nothing
    Just scr -> do
      let progr = mandelbrot -- TODO: make it depend on the bootstrap
          mod   = bootModel b
          fview = StandardView scr
          fctrl = FractalController progr mod fview
      zoom <- tryCreateZoomArea (bootWidth b) (bootHeight b) 0.5
      case zoom of 
        Nothing   -> return Nothing
        Just zoom -> do
          let guictrl = GUIController True zoom
          return Just $ AppController scr fctrl guictrl

runCtrl :: AppController -> IO ()
runCtrl app = do
  showCursor False
  enableKeyRepeat 200 10
  -- TODO: make the first render
  loop app

loop :: AppController -> IO ()
loop app = do
  (continue,newApp) <- handleEvents app
  when continue $ do
    runFractalCtrl (fractalCtrl app)
    runGUICtrl (guiCtrl app)
    SDL.flip $ appScreen app
    loop app

tryGetScreen :: Int -> Int -> Int -> String -> IO (Maybe Surface)
tryGetScreen w h d t = do
  SDL.init [InitVideo]
  screen <- SDL.trySetVideoMode w h d [HWSurface, DoubleBuf]
  SDL.setCaption t [] -- we don’t give a fuck about the title icon
  return screen

tryCreateZoomArea :: Int -> Int -> Float -> IO (Maybe Surface)
tryCreateZoomArea w h zf = do
  let rw = floor $ w / zf
      rh = floor $ h / zf
  zoomArea <- tryCreateRGBSurface [HWSurface] rw rh 32 0 0 0 0
  case zoomArea of
    Nothing -> return Nothing
    Just zoom -> do
      lift $ setAlpha zoomArea [SrcAlpha] 127 -- TODO: Bool, what for?
      pixel <- lift $ mapRGB (surfaceGetPixelFormat zoomArea) 60 60 60
      lift $ fillRect zoomArea Nothing pixel
      return zoom

-- destroy the render
destroyRender :: IO ()
destroyRender = SDL.quit

handleEvents :: AppController -> IO (Bool,AppController)
handleEvents app = do
  event <- pollEvent
  case event of
    NoEvent -> nochange
    Quit    -> quit
    KeyUp k -> case symKey k of
      SDLK_ESCAPE -> quit
      SDLK_SPACE  -> loopback app
      SDLK_RETURN -> loopback app
      SDLK_MINUS  -> loopback app
      SDLK_PLUS   -> loopback app
      _           -> loopback app
    KeyDown k -> case symKey k of
      SDLK_LEFTPAREN  -> loopback app
      SDLK_RIGHTPAREN -> loopback app
      _               -> loopback app
    MouseButtonUp x y b -> case b of
      ButtonLeft -> loopback app
      _          -> loopback app
    _ -> loopback app
 where quit     = return (False,app)
       nochange = return (True,app)
       loopback = handleEvents
