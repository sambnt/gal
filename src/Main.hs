{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when, forM_, foldM, void)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket)
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as BSC
import Foreign.C (peekCString)
import Numeric.Natural (Natural)
import Gal.Window.SDL ( withSDL, withSDLWindow, withWindowEvents )
import Gal.Event (Event(..))
import System.Exit (exitSuccess)
import Gal.Render (withRenderer, executeDrawCalls, generateDrawCalls)
import Gal.Texture (loadTextureAtlas)
import qualified SDL
import qualified SDL.Raw as SDLRaw
import qualified Control.Concurrent.STM as STM
import Data.Ratio ((%))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Foreign.C (CInt)
import Gal.Game (initialGameState)
import Gal.Game.Types (Game, GameEvent(PlayerMoved, CoinStolen, Restart))
import Gal.Time (mkTime, getDeltaTime)
import Gal.Input (translateGameEvents)
import qualified Gal.Game as Game
import Gal.App (App)
import qualified Gal.App as App

main :: IO ()
main = do
  let initialWidth = 800
      initialHeight = 600
  withSDLWindow initialWidth initialHeight "gal" $ \win -> do
    SDLRaw.showCursor 0
    withRenderer win $ \ren -> do
      textureAtlas <- loadTextureAtlas ren

      let app = App.init

      time <- mkTime

      void $ loop app $ \s -> do
        withWindowEvents win $ \evs -> do
          dt <- getDeltaTime time

          s' <- maybe exitSuccess pure $ App.step dt evs s

          let drawCalls = generateDrawCalls textureAtlas (App.appGame s')
          executeDrawCalls ren drawCalls

          pure s'

loop :: App -> (App -> IO App) -> IO App
loop s0 f = do
  s1 <- f s0
  loop s1 f
