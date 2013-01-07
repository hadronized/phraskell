module Phraskell where

import Control.Monad
import Control.Monad.Trans.Maybe
import Fractal
import Graphics.UI.SDL as SDL
import Render
import System.Environment
import System.Console.GetOpt

-- TODO: add fullscreen support
data App = App {
  appWidth :: Float,
  appHeight :: Float,
  appRX :: Float,
  appRY :: Float,
  appZoom :: Float,
  appEquation :: Equation,
  appIterFrame :: IterFrame
  }

data Flag
  = FVersion       -- version of the program
  | FFullscreen    -- should the app in fullscreen mode?
  | FWidth String  -- width of the screen
  | FHeight String -- heigth of the sceen
  | FRX String     -- relative x value
  | FRY String     -- relative y value
  | FZoom String   -- zoom value

options :: [OptDescr Flag]
options =
  [ Option ['v','?'] ["version", "about"] (NoArg FVersion)           "show version"
  , Option ['w']     ["width"]            (ReqArg FWidth "WIDTH")    "width of window"
  , Option ['h']     ["heigth"]           (ReqArg FHeight "HEIGHT")  "height of the window"
  , Option ['x']     ["rx","relx"]        (ReqArg FRX "RELX")        "x displacement"
  , Option ['y']     ["ry","rely"]        (ReqArg FRY "RELY")        "y displacement"
  , Option ['z']     ["zoom"]             (ReqArg FZoom "ZOOM")      "zoom factor"
  ]

parseOpts :: [String] -> MaybeT IO ([Flag], [String])
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return (o,n)
    (_,_,errs) -> mzero

main = do
  screen <- tryGetScreen width height depth title
  case screen of
    Just s -> loop s
    _      -> return ()
  putStrLn "Bye!"

  where
    width  = 800
    height = 600
    depth  = 32
    title  = "Phraskell"
    
    loop s = do
      quit <- treatEvents s
      unless quit $ loop s

-- events handler
treatEvents :: Surface -> IO Bool
treatEvents screen = do
  event <- waitEvent
  case event of
    NoEvent  -> return False
    Quit     -> return True
    KeyUp k  -> case symKey k of
       SDLK_ESCAPE -> return True
       SDLK_RETURN -> return True
       _           -> treatEvents screen
    _        -> treatEvents screen
