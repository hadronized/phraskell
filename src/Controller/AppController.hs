module Controller.AppController where

import Controller.CLI
import Model.Fractal
import View.Fractal

data AppController = AppController {
    fractaModel :: FractalModel
  , fractalView :: FractalView
  --, other controllers here
}

fromBootstrap :: Bootstrap -> AppController
fromBootstrap b =
  let model = 
      view  =
