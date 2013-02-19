module View.GUI where

import Graphics.UI.SDL

data GUIView
  = StandardView {
        stdViewScreen   :: Surface
      , stdViewZoomArea :: Surface
    }

expose :: GUIView -> IO ()
expose view = return () -- TODO: to implement
