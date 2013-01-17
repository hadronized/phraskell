import Control.Monad
import Control.Monad.State
import Equations
import Fractal
import Graphics.UI.SDL as SDL
import Render
import System.Environment
import System.Console.GetOpt
import System.IO

width,height,depth :: Float
title :: String
width  = 800
height = 600
depth  = 32
title  = "Phraskell"

-- TODO: add fullscreen support
data App = App {
    appWidth :: Float
  , appHeight :: Float
  , appRX :: Float
  , appRY :: Float
  , appZoom :: Float
  , appEquation :: Equation
  , appIterFrame :: IterFrame
  , appScreen :: Surface
  }
  
instance Show App where
  show app = "[" ++ show (appWidth app) ++ ","
                 ++ show (appHeight app) ++ ","
                 ++ show (appRX app) ++ ","
                 ++ show (appRY app) ++ ","
                 ++ show (appZoom app) ++ "]"

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

usage :: IO ()
usage = putStrLn $ usageInfo "usage: phraskell [OPTIONS]" options

options :: [OptDescr Flag]
options =
  [ Option ['v','?'] ["version", "about"] (NoArg FVersion)           "show version"
  , Option ['w']     ["width"]            (ReqArg FWidth "WIDTH")    "width of window"
  , Option ['h']     ["heigth"]           (ReqArg FHeight "HEIGHT")  "height of the window"
  , Option ['x']     ["rx","relx"]        (ReqArg FRX "RELX")        "x displacement"
  , Option ['y']     ["ry","rely"]        (ReqArg FRY "RELY")        "y displacement"
  , Option ['z']     ["zoom"]             (ReqArg FZoom "ZOOM")      "zoom factor"
  ]

parseOpts :: [String] -> IO (Maybe ([Flag],[String]))
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return $ Just (o,n)
    (_,_,errs) -> return Nothing

initApp :: App -> ([Flag],[String]) -> App
initApp app (flags,_) = do
  app `initWithFlags` flags
    where initWithFlags a f = foldl modifyAppWithOpt a f

-- alter an application regarding an option flag
modifyAppWithOpt :: App -> Flag -> App
modifyAppWithOpt app f = case f of
  FWidth s  -> app { appWidth = read s }
  FHeight s -> app { appHeight = read s }
  FRX s     -> app { appRX = read s }
  FRY s     -> app { appRX = read s }
  FZoom s   -> app { appZoom = read s }
  _         -> app

-- entry point
main = do
  hSetBuffering stdout NoBuffering

  args <- getArgs -- get the CLI options
  maybeCliOpts <- parseOpts args -- maybe get the options
  case maybeCliOpts of
    Nothing -> usage
    Just flags  -> do
      -- TODO: not so fast, we have to check if there’s not version flag
      -- here we have options, so let’s create our very first application ! but before, create the screen
      withInit [InitVideo] $ do
        maybeScreen <- trySetVideoMode (floor width) (floor height) (floor depth) [HWSurface,DoubleBuf]
        case maybeScreen of
          Nothing -> putStrLn "unable to get a screen! :("
          Just screen -> do
            let app = initApp (App width height 0.0 0.0 1.0 mandelbrotEquation [] screen) flags -- the application
            loop app
            putStrLn "Bye!"
              where loop app = do
                      (quit,newApp) <- treatEvents app
                      SDL.flip $ appScreen app
                      unless quit $ loop newApp

-- events handler
treatEvents :: App -> IO (Bool,App)
treatEvents app = do
  event <- waitEvent

  case event of
    NoEvent  -> nochange
    Quit     -> nochange
    MouseMotion x y rx ry -> onMouseMotion (fromIntegral x) (fromIntegral y)
    KeyUp k  -> case symKey k of
      SDLK_ESCAPE -> quit
      SDLK_RETURN -> quit
      SDLK_u      -> update
      _           -> treatEvents app
    _        -> treatEvents app
 where nochange = return $ (False,app)
       quit     = return $ (True,app)
       onMouseMotion mx my = do
         -- first, create the Rect that defines the cursor position according to the mouse position
         let zf = appZoom app
             rw = appWidth app / zf
             rh = appHeight app / zf
             rx = mx - rw / 2
             ry = my - rh / 2
         maybeZoomSurf <- tryCreateRGBSurface [HWSurface] (floor rw) (floor rh) 32 0 0 0 0
         case maybeZoomSurf of
           Nothing ->
           Just zoomSurf -> do
             setAlpha zoomSurf [SrcAlpha] 127 -- TODO: Bool, what for?
         nochange
       update = do
         putStr $ "updating fractal " ++ show app ++ "... "
         -- the first thing to do is to compute the actual fractal equation for each pixel
         let iterf = mkIterFrame (appWidth app) (appHeight app) (appRX app) (appRY app) (appZoom app) (appEquation app)
         -- then, update the screen regarding the brand new iterframe
         pixelizeSurface iterf (appScreen app)
         putStrLn "done!"
         -- and finaly, returned the altered application
         return (False,app { appIterFrame = iterf })
