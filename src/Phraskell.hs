import Control.Monad
import Control.Monad.State
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

parseOpts :: [String] -> IO (Maybe ([Flag],[String]))
parseOpts args =
  case getOpt Permute options args of
    (o,n,[])   -> return $ Just (o,n)
    (_,_,errs) -> return Nothing

initApp :: App -> ([Flag],[String]) -> Maybe AppState
initApp app (flags,_) = do
    return $ do
      put $ app `initWithFlags` flags
      return True 
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
  -- get the options
  args <- getArgs
  maybeCliOpts <- parseOpts args
  case maybeCliOpts of
    Nothing -> return ()
    Just x  -> withOpts x
  putStrLn "Bye!"

  where
    withOpts cliOpts = do
      -- create and init the application
      let app = App width height 0 0 1 mandelbrotEquation []
      maybeAppState <- lift $ initApp app cliOpts
      case maybeAppState of
        Nothing -> return ()
        Just x  -> withAppState x

    withAppState app = do
      screen <- tryGetScreen width height depth title
      case screen of
        Just s -> loop app
        _      -> return ()

    loop a = do
      newApp <- treatEvents a
      let (quit,_) = runState newApp
      unless quit $ loop newApp

    width  = 800.0
    height = 600.0
    depth  = 32.0
    title  = "Phraskell"

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
