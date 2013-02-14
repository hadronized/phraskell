module Controller.AppController where

import Controller.CLI

data AppController = AppController {
    fractaModel :: FractalModel
  , fractalView :: FractalView
  --, other controllers here
}
