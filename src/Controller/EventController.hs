module Controller.EventController where

import Graphics.UI.SDL

handleEvents :: AppController -> IO (Bool,AppController)
handleEvents app = do
  event <- pollEvent
  case event of
    NoEvent -> nochange
    Quit    -> quit
    KeyUp k -> case symKey k of
      SDLK_ESCAPE -> quit
      SDLK_SPACE  -> loopback app
      SDLK_RETURN -> loopback app
      SDLK_MINUS  -> loopback app
      SDLK_PLUS   -> loopback app
      _           -> loopback app
    KeyDown k -> case symKey k of
      SDLK_LEFTPAREN  -> loopback app
      SDLK_RIGHTPAREN -> loopback app
      _               -> loopback app
    MouseButtonUp x y b -> case b of
      ButtonLeft -> loopback app
      _          -> loopback app
    _ -> loopback app
 where quit     = return (False,app)
       nochange = return (True,app)
       loopback = handleEvents
