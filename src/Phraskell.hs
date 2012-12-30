import Fractal
import Render

main = do
  screen <- initRender width height depth "Phraskell"

  destroyRender

  where
    width  = 800
    height = 600
    depth  = 32
