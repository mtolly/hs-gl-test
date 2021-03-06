{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NegativeLiterals #-}
module Main where

import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Exception
import Control.Concurrent
import Graphics.GL.Core33
import Graphics.GL.Types

-- | Returns Just an event if there is one currently in the queue.
pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> fmap Just $ peek pevt
  _ -> return Nothing

untilNothing :: IO (Maybe a) -> IO [a]
untilNothing act = act >>= \case
  Nothing -> return []
  Just x  -> fmap (x :) $ untilNothing act

-- | Extracts and throws an SDL error if the action returns a null pointer.
notNull :: IO (Ptr a) -> IO (Ptr a)
notNull act = do
  p <- act
  if p == nullPtr
    then SDL.getError >>= peekCString >>= error . ("SDL null pointer: " ++)
    else return p

withSDL :: [SDL.InitFlag] -> IO a -> IO a
withSDL flags = bracket_ (zero $ SDL.init $ foldr (.|.) 0 flags) SDL.quit

-- | Extracts and throws an SDL error if the action doesn't return zero.
zero :: (Eq a, Num a) => IO a -> IO ()
zero act = do
  n <- act
  unless (n == 0) $ SDL.getError >>= peekCString >>= error . ("SDL non-zero code: " ++)

checkGL :: IO ()
checkGL = do
  err <- glGetError
  if err == GL_NO_ERROR
    then return ()
    else error $ "OpenGL error: " ++ show err

fill :: (Storable a) => (Ptr a -> IO ()) -> IO a
fill f = alloca $ \p -> f p >> peek p

fillMany :: (Storable a, Num b) => Int -> (b -> Ptr a -> IO ()) -> IO [a]
fillMany i f = allocaArray i $ \p -> f (fromIntegral i) p >> peekArray i p

loadShaders :: FilePath -> FilePath -> IO GLuint
loadShaders vertfp fragfp = do

  vertShaderID <- glCreateShader GL_VERTEX_SHADER
  fragShaderID <- glCreateShader GL_FRAGMENT_SHADER

  vert <- readFile vertfp
  frag <- readFile fragfp

  let check
        :: (GLuint -> GLenum -> Ptr GLint -> IO ())
        -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
        -> GLuint
        -> IO ()
      check lenFn logFn checkID = do
        len <- fill $ lenFn checkID GL_INFO_LOG_LENGTH
        when (len > 0) $ allocaArray0 (fromIntegral len) $ \p -> do
          logFn checkID (fromIntegral len) nullPtr p
          peekCString p >>= putStrLn

  withCString vert $ \p -> withArrayLen [p] $ \len pp -> do
    glShaderSource vertShaderID (fromIntegral len) pp nullPtr
    glCompileShader vertShaderID
  putStrLn "Checking vertex shader..."
  check glGetShaderiv glGetShaderInfoLog vertShaderID

  withCString frag $ \p -> withArrayLen [p] $ \len pp -> do
    glShaderSource fragShaderID (fromIntegral len) pp nullPtr
    glCompileShader fragShaderID
  putStrLn "Checking fragment shader..."
  check glGetShaderiv glGetShaderInfoLog fragShaderID

  progID <- glCreateProgram
  glAttachShader progID vertShaderID
  glAttachShader progID fragShaderID
  glLinkProgram progID
  putStrLn "Checking program..."
  check glGetProgramiv glGetProgramInfoLog progID

  glDeleteShader vertShaderID
  glDeleteShader fragShaderID
  return progID

main :: IO ()
main = withSDL [SDL.SDL_INIT_VIDEO] $ do
  zero $ SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MAJOR_VERSION 3
  zero $ SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MINOR_VERSION 3
  zero $ SDL.glSetAttribute SDL.SDL_GL_CONTEXT_PROFILE_MASK SDL.SDL_GL_CONTEXT_PROFILE_CORE
  window <- notNull $ withCString "Hello SDL+GL!" $ \str ->
    SDL.createWindow str
      SDL.SDL_WINDOWPOS_UNDEFINED
      SDL.SDL_WINDOWPOS_UNDEFINED
      640
      480
      (SDL.SDL_WINDOW_OPENGL .|. SDL.SDL_WINDOW_SHOWN)
  _ctx <- notNull $ SDL.glCreateContext window
  zero $ SDL.glSetSwapInterval 1

  -- dark blue background
  glClearColor 0 0 0.4 0

  vertexArrayID <- fill $ glGenVertexArrays 1
  glBindVertexArray vertexArrayID

  -- create and compile program from the shaders
  programID <- loadShaders "vshader.vertexshader" "fshader.fragmentshader"

  let bufData = [-1, -1, 0, 1, -1, 0, 0, 1, 0] :: [GLfloat]

  vertexBuffer <- fill $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vertexBuffer
  withArrayLen bufData $ \len p -> do
    let size = fromIntegral $ len * sizeOf (head bufData)
    glBufferData GL_ARRAY_BUFFER size (castPtr p) GL_STATIC_DRAW

  -- the game loop
  fix $ \loop -> do
    -- clear the screen
    glClear GL_COLOR_BUFFER_BIT

    -- use our shader
    glUseProgram programID

    -- 1rst attribute buffer : vertices
    glEnableVertexAttribArray 0
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    glVertexAttribPointer
      0
      3
      GL_FLOAT
      GL_FALSE
      0
      nullPtr

    -- draw the triangle!
    glDrawArrays GL_TRIANGLES 0 3

    glDisableVertexAttribArray 0

    -- swap buffers
    SDL.glSwapWindow window

    events <- untilNothing pollEvent
    unless (any isQuit events) $ do
      threadDelay 5000
      loop

isQuit :: SDL.Event -> Bool
isQuit SDL.QuitEvent{} = True
isQuit _               = False
