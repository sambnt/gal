{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when, forM_)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket)
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as BSC
import Foreign.C (peekCString)
import Numeric.Natural (Natural)
import Gal.Window.SDL ( withSDL, withSDLWindow, withWindowEvents )
import Gal.Event (Event(..))
import System.Exit (exitSuccess)

main :: IO ()
main = do
  withSDL $ do
    let
      initialWidth = 800
      initialHeight = 600
    withSDLWindow initialWidth initialHeight "gal" $ \win -> do
      let loop f = f >> loop f
      loop $ do
        withWindowEvents win $ \evs -> do
          when (not $ null evs) $
            print evs
          forM_ evs processEvent
          threadDelay 10000

processEvent EventQuit = exitSuccess
processEvent _ = pure ()
