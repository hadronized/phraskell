import Application
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Default
import Fractal
import FractalModel
import Graphics.UI.SDL as SDL
import Render
import System.Environment
import System.Console.GetOpt
import System.IO
import UI
import UI.Impl
import Viewer

-- application defaults
width,height,depth :: Float
title :: String
width  = 800
height = 600
depth  = 32
title  = "Phraskell"

-- CLI flag used to customize the applications behavior
data CLIFlag
  = CLIVersion       -- version of the program
  | CLIFullscreen    -- should the app in fullscreen mode?
  | CLIWidth String  -- width of the screen
  | CLIHeight String -- heigth of the sceen
  | CLIX String      -- x value
  | CLIY String      -- y value
  | CLIZoom String   -- zoom value

-- Display some usage informantion on standard output
usage :: IO ()
usage = putStrLn $ usageInfo "usage: phraskell [OPTIONS]" options

-- All possible CLI options
options :: [OptDescr CLIFlag]
options =
  [ Option ['v','?'] ["version", "about"] (NoArg CLIVersion)           "show version"
  , Option ['w']     ["width"]            (ReqArg CLIWidth "WIDTH")    "width of window"
  , Option ['h']     ["heigth"]           (ReqArg CLIHeight "HEIGHT")  "height of the window"
  , Option ['x']     ["rx","relx"]        (ReqArg CLIX "X")            "x displacement"
  , Option ['y']     ["ry","rely"]        (ReqArg CLIY "Y")            "y displacement"
  , Option ['z']     ["zoom"]             (ReqArg CLIZoom "ZOOM")      "zoom factor"
  ]

-- Parse options and maybe return a tuple of filled flags and non-options
parseOpts :: [String] -> MaybeT IO [CLIFlag]
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return o
    _          -> mzero

-- Create an application using the given screen, fractal surface and cli flags
mkApp :: Surface -> Surface -> [CLIFlag] -> App
mkApp scr fractalSurface flags = do
  (App def (IterFrame []) scr fractalSurface) `initWithFlags` flags
    where initWithFlags a f = foldl modifyAppWithOpt a f

-- Alter an application regarding an option flag
modifyAppWithOpt :: App -> CLIFlag -> App
modifyAppWithOpt app f = app { appViewer = newViewer }
  where viewer = appViewer app
        newViewer = case f of
          CLIWidth s  -> viewer { viewerWidth = read s }
          CLIHeight s -> viewer { viewerHeight = read s }
          CLIX s      -> viewer { viewerX = read s }
          CLIY s      -> viewer { viewerX = read s }
          CLIZoom s   -> viewer { viewerZoom = read s }
          _           -> viewer

-- Entry point
main = do
  hSetBuffering stdout NoBuffering

  args <- getArgs
  params <- runMaybeT $ do
             cliOpts <- (parseOpts args) 
             maybeScreen <- MaybeT $ trySetVideoMode (floor width) (floor height) (floor depth) [HWSurface,DoubleBuf]
             scr <- MaybeT $ trySetVideoMode (floor width) (floor height) (floor depth) [HWSurface,DoubleBuf]
             fractalSurface <- MaybeT $ tryCreateRGBSurface [HWSurface] (floor width) (floor height) (floor depth) 0 0 0 0
             return (cliOpts,scr,fractalSurface)
  case params of
    Just (cliOpts,scr,fractalSurface) -> launch $ mkApp scr fractalSurface cliOpts
    _                                 -> putStrLn "something just went wrong! :("
  print "Bye!"
    where launch app = do
            onFractalFrameUpdate app >>= loop
          loop app = do
            SDL.flip $ appScreen app
            (goon,newApp) <- treatEvents app
            when goon $ loop newApp
