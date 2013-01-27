import Control.Monad
import Control.Monad.State
import Equations
import Fractal
import Graphics.UI.SDL as SDL
import Render
import System.Environment
import System.Console.GetOpt
import System.IO

-- application defaults
width,height,depth :: Float
title :: String
width  = 800
height = 600
depth  = 32
title  = "Phraskell"

-- TODO: add fullscreen support
data App = App {
    appWidth :: Float           -- width of the window (and subsequently the fractal frame)
  , appHeight :: Float          -- height of the window (ditto)
  , appRX :: Float              -- relative x offset of the viewer
  , appRY :: Float              -- relative y offset of the viewer
  , appZoom :: Float            -- zoom factor of the viewer
  , appEquation :: Equation     -- equation to render
  , appIterFrame :: IterFrame   -- iteration frame
  , appScreen :: Surface        -- screen surface
  , appFractalFrame :: Surface  -- fractal surface
  }

instance Show App where
  show app = "[" ++ show (appWidth app) ++ ","
                 ++ show (appHeight app) ++ ","
                 ++ show (appRX app) ++ ","
                 ++ show (appRY app) ++ ","
                 ++ show (appZoom app) ++ "]"

-- CLI flag used to customize the application’s behavior
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
modifyAppWithOpt app f = case f of
  FWidth s  -> app { appWidth = read s }
  FHeight s -> app { appHeight = read s }
  FRX s     -> app { appRX = read s }
  FRY s     -> app { appRX = read s }
  FZoom s   -> app { appZoom = read s }
  _         -> app

-- Entry point
main = do
  hSetBuffering stdout NoBuffering

  --args <- getArgs -- get the CLI options
  maybeCliOpts <- getArgs >>= parseOpts -- maybe get the options
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

-- Events handler
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
             rw = floor $ appWidth app / zf
             rh = floor $ appHeight app / zf
             rx = mx - rw `div` 2
             ry = my - rh `div` 2
         maybeZoomSurf <- tryCreateRGBSurface [HWSurface] rw rh 32 0 0 0 0
         case maybeZoomSurf of
           Nothing -> nochange
           Just zoomSurf -> do
             setAlpha zoomSurf [SrcAlpha] 127 -- TODO: Bool, what for?
             pixel <- mapRGB (surfaceGetPixelFormat zoomSurf) 60 60 60
             fillRect zoomSurf Nothing pixel
             blitSurface zoomSurf Nothing (appScreen app) (Just $ Rect rx ry rw rh)
             freeSurface $ zoomSurf
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
