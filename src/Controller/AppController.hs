module Controller.AppController where

import Comtroll.Monad (unless)
import Controller.CLI
import Controller.EventController
import Graphics.UI.SDL (enableKeyRepeat,flip,showCursor,Surface) as SDL
import Model.Fractal
import View.Fractal

data AppController = AppController {
    appScreen    :: Surface
  , fractalCtrl :: FractalController
  , guiCtrl     :: GUIController
}

fromBootstrap :: Bootstrap -> AppController
fromBootstrap b =
  let model = bootModel b
      view  = 
  in --AppController model view

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
