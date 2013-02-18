module Controller.GUI where

import View.GUIView

data GUIController = GUIController {
    guiCtrlVisible  :: Bool
  , guiView         :: GUIView
}

