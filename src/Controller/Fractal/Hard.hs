module Controller.Fractal.Hard where

import Control.Monad.Trans.Maybe
import Foreign.C.String
import Graphics.Rendering.OpenGL.Raw

type ShaderProgram = GLuint

vertexShaderStr,fragmentShaderStr :: String
vertexShaderStr = "#version 150\n\
in vec2 co;\n\
out vec2 cardco;\n\

void main() {\n\
  gl_Position = co;\n\
  cardco = co; /* TODO: add offsets and zoom support */
}"
fragmentShaderStr = "#version 150\n\
in vec2 cardco;\n\

void main() {\n\
}"

createShaderProgram :: MaybeT IO ShaderProgram
createShaderProgram = do
  vertexShader <- lift $ glCreateShader gl_VERTEX_SHADER
  guard vertexShader == 0
  fragmentShader <- lift $ glCreateShader gl_FRAGMENT_SHADER
  guard fragmentShader == 0

  glShaderSource vertexShader (fromIntegral 1) 
