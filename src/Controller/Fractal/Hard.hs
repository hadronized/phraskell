module Controller.Fractal.Hard where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Foreign
import Foreign.C.String
import Graphics.Rendering.OpenGL.Raw

-- TODO: add errors handling

type ShaderStage   = GLuint
type ShaderProgram = GLuint

type GLError = GLenum

fromGLError :: GLError -> String
fromGLError e = case e of
  gl_NO_ERROR          -> []
  gl_INVALID_ENUM      -> "invalid enum"
  gl_INVALID_VALUE     -> "invalid value"
  gl_INVALID_OPERATION -> "invalid operation"
  gl_OUT_OF_MEMORY     -> "out of memory"
  _                    -> "unknown error"

{-
newtype IOGL a = IOGL { runIOGL :: IO (Either GLError a) }

instance Monad IOGL where
  return = IOGL . return . return
  IOGL glio >>= f = IOGL $ do
    v <- glio
    case v of
      Left  e -> return $ Left e
      Right x -> do
        y   <- runIOGL $ f x
        err <- glGetError
        if err /= gl_NO_ERROR then return $ Left err else return y
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

{-
createShaderProgram :: EitherT IO String ShaderProgram
createShaderProgram = do
  vs <- EitherT $ createCompileShaderStage gl_VERTEX_SHADER vertexShaderStr
  fs <- EitherT $ createCompileShaderStage gl_FRAGMENT_SHADER fragmentShaderStr
-}

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

createProgram :: IO (Maybe ShaderProgram)
createProgram = do
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
    peek status >>= return . toBool

linkingLog :: ShaderProgram -> IO String
linkingLog sp = do
  with 0 $ \length -> do
    glGetProgramiv sp gl_INFO_LOG_LENGTH length
    bytes <- fromIntegral `liftM` peek length
    allocaBytes bytes $ \linfo -> do
      glGetProgramInfoLog sp (fromIntegral bytes) nullPtr linfo
      str <- peekCString $ castPtr linfo
      return str
