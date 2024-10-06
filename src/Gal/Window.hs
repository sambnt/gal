{-# LANGUAGE ScopedTypeVariables #-}

module Gal.Window ( Window(..)
                  , withWindow
                  , getRequiredExtensions
                  , getFramebufferSize
                    -- , getWindowEvent
                  , withWindowEvent
                  , closeWindow
                  ) where

import Numeric.Natural (Natural)
import qualified Gal.Window.GLFW as GalGLFW
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.ByteString.Char8 as BSC
import Gal.Window.Types (WindowEvent)

data Window = GLFW GalGLFW.WindowGLFW

withWindow
  :: Natural
  -> Natural
  -> Text
  -> (Window -> IO ())
  -> IO ()
withWindow width height title f =
  GalGLFW.withWindow width height title (f . GLFW)

getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize (GLFW w) = GalGLFW.getFramebufferSize w

getRequiredExtensions :: Window -> IO (Vector BSC.ByteString)
getRequiredExtensions (GLFW w) = GalGLFW.getRequiredExtensions w

closeWindow :: Window -> IO ()
closeWindow (GLFW w) = GalGLFW.closeWindow w

-- getWindowEvent :: Window -> IO (Maybe WindowEvent)
-- getWindowEvent (GLFW w) = GalGLFW.getWindowEvent w

withWindowEvent :: Window -> (Maybe WindowEvent -> IO r) -> IO r
withWindowEvent (GLFW w) = GalGLFW.withWindowEvent w
