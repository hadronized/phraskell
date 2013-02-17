module Controller.AppController where

import Comtroll.Monad (unless)
import Controller.CLI
import Controller.EventController
import Controller.GUIController
import Data.Maybe (maybe)
import Graphics.UI.SDL as SDL
import Model.Fractal
import Model.Progression
import View.FractalView

data AppController = AppController {
    appScreen    :: Surface
  , fractalCtrl  :: FractalController
  , guiCtrl      :: GUIController
}

fromBootstrap :: Bootstrap -> IO (Maybe AppController)
fromBootstrap b = do
  -- first construct the view; we need a Surface!
  screen <- tryGetScreen $ (bootWdith b) (bootHeight b) 32 "phraskell"
  case screen of
    Nothing  -> return Nothing
    Just scr -> do
      -- now let’s init the fractal controller
      let progr = mandelbrot -- TODO: make it depend on the bootstrap
          mod   = bootModel b
          fview = StandarView scr
      return $ AppController scr (FractalController progr mod fview) GUIHERE

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
    runFractalCtrl (fractalCtrl app) -- view here?
    runGUICtrl (guiCtrl app) -- view here?
    SDL.flip $ appScreen app
    loop app

tryGetScreen :: Int -> Int -> Int -> String -> IO (Maybe Surface)
tryGetScreen w h d t = do
  SDL.init [InitVideo]
  screen <- SDL.trySetVideoMode w h d [HWSurface, DoubleBuf]
  SDL.setCaption t [] -- we don’t give a fuck about the title icon
  return screen

-- destroy the render
destroyRender :: IO ()
destroyRender = SDL.quit
