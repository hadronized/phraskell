module Controller.GUIController where

import Graphics.UI.SDL

data GUIController = GUIController {
    guiCtrlVisible  :: Bool
  , guiCtrlZoomArea :: Surface
}

createGUICtrl :: 
runGUICtrl :: GUIController -> IO ()
runGUICtrl gui = do

