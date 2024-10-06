module Gal.Window.SDL ( withSDL
                      , withSDLWindow
                      , withWindowEvents
                      , WindowSDL
                      ) where

import qualified SDL
import qualified SDL.Font
import Control.Concurrent.STM (TQueue)
import Control.Exception.Safe (mask, onException, bracket)
import Control.Monad (forM_)
import qualified Control.Concurrent.STM as STM
import Numeric.Natural (Natural)
import Data.Text (Text)

import Gal.Event (Event(..))

data WindowSDL = WindowSDL { win :: SDL.Window
                           , eventQueue :: TQueue Event
                           }

withSDL :: IO a -> IO a
withSDL action =
  bracket
    (SDL.initialize [SDL.InitVideo, SDL.InitEvents])
    (const SDL.quit)
    (const action)

withSDLWindow :: Natural -> Natural -> Text -> (WindowSDL -> IO r) -> IO r
withSDLWindow width height title f = do
   eventQue <- STM.newTQueueIO

   let
     windowSize   = SDL.V2 (fromIntegral width) (fromIntegral height)
     sdlWindowCfg = SDL.defaultWindow
         { SDL.windowInitialSize = windowSize
         , SDL.windowMode        = SDL.Windowed
         , SDL.windowHighDPI     = True
         }

   bracket
     (SDL.createWindow title sdlWindowCfg)
     SDL.destroyWindow
     (\w -> f $ WindowSDL w eventQue)


withWindowEvents :: WindowSDL -> ([Event] -> IO r) -> IO r
withWindowEvents (WindowSDL _ que) f = do
  mask $ \restore -> do
    sdlEvents <-
      restore (fmap SDL.eventPayload <$> SDL.pollEvents)
    forM_ sdlEvents (pushEvent que)
    evs <- STM.atomically $ STM.flushTQueue que
    restore (f evs)
      `onException` STM.atomically (forM_ evs (STM.writeTQueue que))

pushEvent :: TQueue Event -> SDL.EventPayload -> IO ()
pushEvent q sdlEvent = case sdlEvent of
  SDL.QuitEvent ->
    queue q EventQuit
  SDL.KeyboardEvent keyEventData ->
    queue q $ EventKey keyEventData
  SDL.MouseButtonEvent mouseBtnEventData ->
    queue q $ EventMouseButton mouseBtnEventData
  SDL.MouseMotionEvent mouseMtnEventData ->
    queue q $ EventMouseMotion mouseMtnEventData
  SDL.MouseWheelEvent mouseWhlEventData  ->
    queue q $ EventMouseWheel mouseWhlEventData
  SDL.TextInputEvent textEventData ->
    queue q $ EventTextInput textEventData
  _ ->
    pure ()

queue :: TQueue a -> a -> IO ()
queue que = STM.atomically . STM.writeTQueue que
