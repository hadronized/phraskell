import Control.Monad
import Fractal
import Render

main = do
  screen <- tryGetScreen width height depth title
  case screen of
    Just s -> loop s
    _      -> return ()

  where
    width  = 800
    height = 600
    depth  = 32
    title  = "Phraskell"
    
    loop screen = do
      -- first of all, handle events
      forever $ return ()
