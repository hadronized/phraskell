module Controller.Fractal.Hard where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Foreign
import Foreign.C.String
import Graphics.Rendering.OpenGL.Raw

-- TODO: add errors handling

-- Shader Objects part

type ShaderStage   = GLuint
type ShaderProgram = GLuint
type BufferObject  = GLuint
type BufferType    = GLenum

type GLError       = GLenum

{-
fromGLError :: GLError -> String
fromGLError e = case e of
  gl_NO_ERROR          -> []
  gl_INVALID_ENUM      -> "invalid enum"
  gl_INVALID_VALUE     -> "invalid value"
  gl_INVALID_OPERATION -> "invalid operation"
  gl_OUT_OF_MEMORY     -> "out of memory"
  _                    -> "unknown error"
-}

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

getProgram :: EitherT String IO ShaderProgram
getProgram = do
  vs <- EitherT $ createCompileShaderStage gl_VERTEX_SHADER vertexShaderStr
  fs <- EitherT $ createCompileShaderStage gl_FRAGMENT_SHADER fragmentShaderStr
  msp <- lift $ createShaderProgram
  case msp of
    Nothing -> left "unable to get the shader program"
    Just sp -> do
      lift $ attachToProgram sp vs
      lift $ attachToProgram sp fs
      lift $ linkProgram sp
      linked <- lift $ checkLinking sp
      if linked then return sp else (lift $ linkingLog sp) >>= left

createCompileShaderStage :: GLenum -> String -> IO (Either String ShaderStage)
createCompileShaderStage st src = do
  ms <- createShaderStage st
  case ms of
    Nothing -> return $ Left "unable to create shader stage"
    Just s -> do
      compileShaderStage s src
      compiled <- checkCompilation s
      if not compiled
        then compilationLog s >>= return . Left
        else do
          return $ Right s

createShaderStage :: GLenum -> IO (Maybe ShaderStage)
createShaderStage st = do
  stage <- glCreateShader st
  return $ if stage /= 0 then Just stage else Nothing

deleteShaderStage :: ShaderStage -> IO ()
deleteShaderStage = glDeleteShader

compileShaderStage :: ShaderStage -> String -> IO ()
compileShaderStage s src = do
  with 1 $ \nb -> do
    foo <- newCString src
    with foo  $ \csrc -> do
      glShaderSource s (fromIntegral 1) (castPtr csrc) nb

checkCompilation :: ShaderStage -> IO Bool
checkCompilation s = do
  with 0 $ \status -> do
    glGetShaderiv s gl_COMPILE_STATUS status
    toBool `liftM` peek status

compilationLog :: ShaderStage -> IO String
compilationLog s = do
  with 0 $ \length -> do
    glGetShaderiv s gl_INFO_LOG_LENGTH length
    bytes <- fromIntegral `liftM` peek length
    allocaBytes bytes $ \linfo -> do
      glGetShaderInfoLog s (fromIntegral bytes) nullPtr linfo
      str <- peekCString $ castPtr linfo
      return str

createShaderProgram :: IO (Maybe ShaderProgram)
createShaderProgram = do
  sp <- glCreateProgram
  return $ if sp /= 0 then Just sp else Nothing

attachToProgram :: ShaderProgram -> ShaderStage -> IO ()
attachToProgram = glAttachShader

linkProgram :: ShaderProgram -> IO ()
linkProgram sp = do
  glLinkProgram sp

checkLinking :: ShaderProgram -> IO Bool
checkLinking sp = do
  with 0 $ \status -> do
    glGetProgramiv sp gl_LINK_STATUS status
    toBool `liftM` peek status

linkingLog :: ShaderProgram -> IO String
linkingLog sp = do
  with 0 $ \length -> do
    glGetProgramiv sp gl_INFO_LOG_LENGTH length
    bytes <- fromIntegral `liftM` peek length
    allocaBytes bytes $ \linfo -> do
      glGetProgramInfoLog sp (fromIntegral bytes) nullPtr linfo
      str <- peekCString $ castPtr linfo
      return str

useProgram :: ShaderProgram -> IO ()
useProgram = glUseProgram

unuseProgram :: IO ()
unuseProgram = glUseProgram 0

-- Buffers Objects part

genBuffer :: (Storable a) => [a] -> EitherT String IO BufferObject
genBuffer d = do
  buf <- lift createBuffer
  lift $ bindBuffer gl_ARRAY_BUFFER buf -- actually create the buffer here
  lift $ pushBuffer buf (fromIntegral 4) d gl_STATIC_DRAW
  EitherT $ return $ return buf

genVBO :: EitherT String IO BufferObject
genVBO = genBuffer vertices
  where vertices = concat $ [[-1,-1],[-1,1],[1,1],[1,-1]] :: [Int]

genIBO :: EitherT String IO BufferObject
genIBO = genBuffer indices
  where indices = [0,1,2,0,2,3] :: [Int]

createBuffer :: IO BufferObject
createBuffer = do
  alloca $ \bid -> do
    glGenBuffers 1 bid
    peek bid

deleteBuffer :: BufferObject -> IO ()
deleteBuffer b = do
  with b $ \pb -> glDeleteBuffers 1 pb

bindBuffer :: BufferObject -> BufferType -> IO ()
bindBuffer = glBindBuffer

unbindBuffer :: BufferObject -> IO ()
unbindBuffer b = bindBuffer b 0

pushBuffer :: (Storable a ) => BufferObject -> GLsizeiptr -> [a] -> GLenum -> IO ()
pushBuffer b s a u = do
  withArray a $ \d -> do
    glBufferData b s d u

-- texture hihi

type Texture     = GLuint
type TextureType = GLuint

createTexture :: IO Texture
createTexture = do
  alloca $ \tid -> do
    glGenTextures 1 tid
    peek tid

deleteTexture :: Texture -> IO ()
deleteTexture t = do
  with t $ \tid -> do
    glDeleteTextures 1 tid

bindTexture :: TextureType -> Texture -> IO ()
bindTexture = glBindTexture

unbindTexture :: TextureType -> IO ()
unbindTexture t = bindTexture t 0
