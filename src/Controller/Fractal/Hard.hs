module Controller.Fractal.Hard where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.ByteString
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Graphics.Rendering.OpenGL.Raw

type ShaderProgram = GLuint
type ShaderStage   = GLuint

vertexShaderStr,fragmentShaderStr :: String
vertexShaderStr   = "#version 150\n\
                    \in vec2 co;\n\
                    \out vec2 cardco;\n\

                    \void main() {\n\
                    \  gl_Position = co;\n\
                    \  cardco = co; /* TODO: add offsets and zoom support */ \n\
                    \}"
fragmentShaderStr = "#version 150\n\
                    \in vec2 cardco;\n\
                    \out vec4 frag;\n\

                    \void main() {\n\
                    \  frag = vec4(0.f, 0.f, 0.f, 1.f);\n\
                    \}"

{-
createShaderProgram :: MaybeT IO ShaderProgram
createShaderProgram = do
  vertexShader <- lift $ glCreateShader gl_VERTEX_SHADER
  guard $ vertexShader == 0

-}
-- glShaderSource :: GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ()

createShaderStage :: GLenum -> MaybeT IO ShaderStage
createShaderStage st = do
  stage <- lift $ glCreateShader st
  guard $ stage /= 0
  return stage

compileShaderStage :: ShaderStage -> ByteString -> MaybeT IO ()
compileShaderStage s src = do
  nbPtr <- lift $ malloc
  lift $ poke nbPtr (fromIntegral 1 :: Int)
  csrc <- lift $ newCString src
  --glShaderSource s (fromIntegral 1) {- src -} {- number of src -}
  return ()
