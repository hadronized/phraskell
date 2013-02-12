import Application
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Default
import Fractal
import FractalModel
import Graphics.UI.SDL as SDL
import GUI
import Render
import System.Environment
import System.Console.GetOpt
import System.IO
import UI
import UI.Impl
import Viewer

-- application defaults
depth :: Double
depth  = 32

title :: String
title  = "Phraskell"

-- CLI flag used to customize the applications behavior
data CLIFlag
  = CLIVersion           -- version of the program
  | CLIFullscreen        -- should the app in fullscreen mode?
  | CLIWidth String      -- width of the screen
  | CLIHeight String     -- heigth of the sceen
  | CLIX String          -- x value
  | CLIY String          -- y value
  | CLIZoom String       -- zoom value
  | CLIMaxIter Integer   -- max iteration value

-- Display some usage informantion on standard output
usage :: String
usage = usageInfo "usage: phraskell [OPTIONS]" options

-- All possible CLI options
options :: [OptDescr CLIFlag]
options =
  [ Option ['v','?'] ["version", "about"] (NoArg CLIVersion)           "show version"
  , Option ['w']     ["width"]            (ReqArg CLIWidth "WIDTH")    "width of window"
  , Option ['h']     ["heigth"]           (ReqArg CLIHeight "HEIGHT")  "height of the window"
  , Option ['x']     ["rx","relx"]        (ReqArg CLIX "X")            "x displacement"
  , Option ['y']     ["ry","rely"]        (ReqArg CLIY "Y")            "y displacement"
  , Option ['z']     ["zoom"]             (ReqArg CLIZoom "ZOOM")      "zoom factor"
  , Option ['f']     ["fullscreen"]       (NoArg CLIFullscreen)        "launch in fullscreen"
  ]

-- Parse options and maybe return a tuple of filled flags and non-options
parseOpts :: [String] -> MaybeT IO [CLIFlag]
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return o
    _          -> mzero

-- Create a viewer with CLI flags
mkViewer :: [CLIFlag] -> Viewer
mkViewer = foldl alterViewerWithFlag def

-- alter a viewer regarding an option flag
alterViewerWithFlag :: Viewer -> CLIFlag -> Viewer
alterViewerWithFlag v f = case f of
  CLIWidth s    -> v { viewerWidth = read s }
  CLIHeight s   -> v { viewerHeight = read s }
  CLIX s        -> v { viewerX = read s }
  CLIY s        -> v { viewerX = read s }
  CLIZoom s     -> v { viewerZoom = read s }
  CLIMaxIter i  -> v { viewerMaxIter = i }
  CLIFullscreen -> v { viewerFullscreen = True }
  _             -> v

-- Entry point
main = do
  hSetBuffering stdout NoBuffering

  args <- getArgs
  params <- runMaybeT $ do
              cliOpts        <- (parseOpts args) 
              viewer         <- return $ mkViewer cliOpts
              maybeScreen    <- MaybeT $ trySetVideoMode (floor $ viewerWidth viewer) (floor $ viewerHeight viewer) (floor depth) ([HWSurface,DoubleBuf] ++ if viewerFullscreen viewer then [Fullscreen] else [])
              scr            <- MaybeT $ trySetVideoMode (floor $ viewerWidth viewer) (floor $ viewerHeight viewer) (floor depth) [HWSurface,DoubleBuf]
              fractalSurface <- MaybeT $ tryCreateRGBSurface [HWSurface] (floor $ viewerWidth viewer) (floor $ viewerHeight viewer) (floor depth) 0 0 0 0
              gui            <- createGUI viewer
              return (viewer,scr,fractalSurface,gui)
  case params of
    Nothing -> putStrLn usage
    Just (viewer,scr,fractalSurface,gui) -> do
      let app = App viewer (IterFrame []) scr fractalSurface True gui
      launch app
        where launch app = do
                showCursor False
                enableKeyRepeat 200 10
                onFractalFrameUpdate app >>= loop
              loop app = do
                (goon,newApp) <- treatEvents app
                case goon of
                  False -> return ()
                  True  -> do
                    blitSurface (appFractalFrame app) Nothing (appScreen app) Nothing
                    (mx,my,_) <- getMouseState
                    when (appVisibleGUI app) $ renderGUI app mx my
                    SDL.flip $ appScreen app
                    delay 10
                    loop newApp
