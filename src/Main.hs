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
import Control.Concurrent.STM
import Graphics.GL.Standard21

-- | Returns Just an event if there is one currently in the queue.
pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> fmap Just $ peek pevt
  _ -> return Nothing

-- | Extracts and throws an SDL error if the action returns a null pointer.
notNull :: IO (Ptr a) -> IO (Ptr a)
notNull act = do
  p <- act
  if p == nullPtr
    then SDL.getError >>= peekCString >>= error
    else return p

withSDL :: [SDL.InitFlag] -> IO a -> IO a
withSDL flags = bracket_ (zero $ SDL.init $ foldr (.|.) 0 flags) SDL.quit

-- | Extracts and throws an SDL error if the action doesn't return zero.
zero :: (Eq a, Num a) => IO a -> IO ()
zero act = do
  n <- act
  unless (n == 0) $ SDL.getError >>= peekCString >>= error

checkGL :: IO ()
checkGL = do
  err <- glGetError
  if err == GL_NO_ERROR
    then return ()
    else error $ "OpenGL error: " ++ show err

main :: IO ()
main = withSDL [SDL.SDL_INIT_VIDEO] $ do
  zero $ SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MAJOR_VERSION 2
  zero $ SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MINOR_VERSION 1
  window <- notNull $ withCString "Hello SDL+GL!" $ \str ->
    SDL.createWindow str
      SDL.SDL_WINDOWPOS_UNDEFINED
      SDL.SDL_WINDOWPOS_UNDEFINED
      640
      480
      (SDL.SDL_WINDOW_OPENGL .|. SDL.SDL_WINDOW_SHOWN)
  ctx <- notNull $ SDL.glCreateContext window
  zero $ SDL.glSetSwapInterval 1
  -- initGL
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  checkGL
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
  checkGL
  glClearColor 0 0 0 1
  checkGL
  -- make event queue
  queue <- newTVarIO []
  _ <- forkIO $ forever $ pollEvent >>= \case
    Nothing -> return ()
    Just e -> atomically $ modifyTVar queue (e :)
  -- the game loop
  fix $ \loop -> do
    glClear GL_COLOR_BUFFER_BIT
    glBegin GL_QUADS
    glVertex2f -0.5 -0.5
    glVertex2f  0.5 -0.5
    glVertex2f  0.5  0.5
    glVertex2f -0.5  0.5
    glEnd
    SDL.glSwapWindow window
    events <- fmap reverse $ atomically $ swapTVar queue []
    unless (any isQuit events) $ do
      threadDelay 5000
      loop

isQuit :: SDL.Event -> Bool
isQuit SDL.QuitEvent{} = True
isQuit _               = False
