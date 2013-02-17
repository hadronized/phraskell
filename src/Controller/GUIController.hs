module Controller.GUIController where

import Graphics.UI.SDL

data GUIController = GUIController {
    guiCtrlVisible  :: Bool
  , guiCtrlZoomArea :: Surface
}

