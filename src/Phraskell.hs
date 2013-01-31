import Application
import Control.Monad
import Control.Monad.State
import Equations
import Fractal
import FractalModel
import Graphics.UI.SDL as SDL
import Render
import System.Environment
import System.Console.GetOpt
import System.IO
import UI
import Viewer

-- application defaults
width,height,depth :: Float
title :: String
width  = 800
height = 600
depth  = 32
title  = "Phraskell"

-- CLI flag used to customize the applications behavior
data Flag
  = FVersion       -- version of the program
  | FFullscreen    -- should the app in fullscreen mode?
  | FWidth String  -- width of the screen
  | FHeight String -- heigth of the sceen
  | FRX String     -- relative x value
  | FRY String     -- relative y value
  | FZoom String   -- zoom value

-- Display some usage informantion on standard output
usage :: IO ()
usage = putStrLn $ usageInfo "usage: phraskell [OPTIONS]" options

-- All possible CLI options
options :: [OptDescr Flag]
options =
  [ Option ['v','?'] ["version", "about"] (NoArg FVersion)           "show version"
  , Option ['w']     ["width"]            (ReqArg FWidth "WIDTH")    "width of window"
  , Option ['h']     ["heigth"]           (ReqArg FHeight "HEIGHT")  "height of the window"
  , Option ['x']     ["rx","relx"]        (ReqArg FRX "RELX")        "x displacement"
  , Option ['y']     ["ry","rely"]        (ReqArg FRY "RELY")        "y displacement"
  , Option ['z']     ["zoom"]             (ReqArg FZoom "ZOOM")      "zoom factor"
  ]

-- Parse options and maybe return a tuple of filled flags and non-options
parseOpts :: [String] -> IO (Maybe ([Flag],[String]))
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return $ Just (o,n)
    (_,_,errs) -> return Nothing

-- Initialize the given application with CLI options
initApp :: App -> ([Flag],[String]) -> App
initApp app (flags,_) = do
  app `initWithFlags` flags
    where initWithFlags a f = foldl modifyAppWithOpt a f

-- Alter an application regarding an option flag
modifyAppWithOpt :: App -> Flag -> App
modifyAppWithOpt app f = app { appViewer = newViewer }
  where viewer = appViewer app
        newViewer = case f of
          FWidth s  -> viewer { viewerWidth = read s }
          FHeight s -> viewer { viewerHeight = read s }
          FRX s     -> viewer { viewerX = read s }
          FRY s     -> viewer { viewerX = read s }
          FZoom s   -> viewer { viewerZoom = read s }
          _         -> viewer

-- Entry point
main = do
  hSetBuffering stdout NoBuffering

  --args <- getArgs -- get the CLI options
  maybeCliOpts <- getArgs >>= parseOpts -- maybe get the options
  case maybeCliOpts of
    Nothing -> usage
    Just flags  -> do
      -- TODO: not so fast, we have to check if there¿s not version flag
      -- here we have options, so let¿s create our very first application ! but before, create the screen
      withInit [InitVideo] $ do
        maybeScreen <- trySetVideoMode (floor width) (floor height) (floor depth) [HWSurface,DoubleBuf]
        case maybeScreen of
          Nothing     -> putStrLn "unable to get a screen! :("
          Just screen -> do
            let viewer = Viewer width height 1.0 0.0 0.0 0 mandelbrotEquation
            maybeFractalSurface <- tryCreateRGBSurface [HWSurface] (floor width) (floor height) (floor depth) 0 0 0 0
            case maybeFractalSurface of
              Nothing             -> putStrLn "unable to get a fractal surface! :("
              Just fractalSurface -> do
                let app = initApp (App viewer (IterFrame []) screen fractalSurface) flags -- the application
                loop app
                putStrLn "Bye!"
                  where loop app = do
                          (quit,newApp) <- treatEvents app
                          SDL.flip $ appScreen app
                          unless quit $ loop newApp
