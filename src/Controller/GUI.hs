module Controller.GUIController where

import View.GUIView

data GUIController = GUIController {
    guiCtrlVisible  :: Bool
  , guiView         :: GUIView
}

