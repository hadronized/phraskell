module Controller.AppController where

import Controller.CLI
import Controller.EventController
import Graphics.UI.SDL (enableKeyRepeat,showCursor)
import Model.Fractal
import View.Fractal

data AppController = AppController {
    fractaModel :: FractalModel
  , fractalView :: FractalView
  --, other controllers here
}

fromBootstrap :: Bootstrap -> AppController
fromBootstrap b =
  let model = bootModel b
      view  = 
  in AppController model view

runCtrl :: AppController -> IO ()
runCtrl app = do
  showCursor False
  enableKeyRepeat 200 10
  -- make the first render

  
