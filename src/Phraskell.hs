module Phraskell where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Equations
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
  
-- the state of the application is its parameters (App) and
-- a boolean that states if it’s running 
type AppState = State App Bool

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


createApp :: AppState
createApp = put App 800.0 600.0 0.0 0.0 1.0 mandelbrotEquation [[]]

parseOpts :: [String] -> MaybeT IO ([Flag],[String])
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return (o,n)
    (_,_,errs) -> mzero

{-
initApp :: MaybeT IO ([Flag],[String]) -> MaybeT IO AppState
initApp maybeOpts = do
  opts <- runMaybeT maybeOpts
  maybeFlag <- opts
  case maybeFlag of
    Just x = return $ initWithFlags x
    _      = mzero
    where initWithFlags 
    -}

main = do
  let app = createApp
  screen <- tryGetScreen width height depth title
  case screen of
    Just s -> loop app
    _      -> return ()
  putStrLn "Bye!"

  where
    width  = 800
    height = 600
    depth  = 32
    title  = "Phraskell"
    
    loop a = do
      newApp <- treatEvents a
      unless quit $ loop newApp

-- events handler
treatEvents :: AppState -> IO AppState
treatEvents app = do
  event <- waitEvent
  case event of
    NoEvent  -> return False
    Quit     -> return True
    KeyUp k  -> case symKey k of
       SDLK_ESCAPE -> app >>= return True
       SDLK_RETURN -> app >>= return True
       _           -> treatEvents app
    _        -> treatEvents app
