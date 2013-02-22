module View.GUI where

import Graphics.UI.SDL

data GUIView
  = StandardView {
        stdViewScreen   :: Surface
      , stdViewZoomArea :: Surface
    }
