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
import Gal.Game (Game, initialGameState, GameEvent(PlayerMoved, CoinStolen, Restart))
import qualified Gal.Game as Game

data State = State { stateGame :: Game
                   , stateUnhandledEvents :: [GameEvent]
                   }
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  let
    initialWidth = 800
    initialHeight = 600
    initialState = State { stateGame = initialGameState
                         , stateUnhandledEvents = []
                         }
  withSDLWindow initialWidth initialHeight "gal" $ \win -> do
    SDLRaw.showCursor 0
    withRenderer win $ \ren -> do
      textureAtlas <- loadTextureAtlas ren

      time <- STM.newTVarIO =<< getCurrentTime

      let
        loop :: State -> (State -> IO State) -> IO State
        loop s0 f = do
            s1 <- f s0
            loop s1 f
      void $ loop initialState $ \s -> do
        withWindowEvents win $ \evs -> do
          when (not $ null evs) $
            print evs
          t1 <- getCurrentTime
          dt <- STM.atomically $ do
            t0 <- STM.readTVar time
            STM.writeTVar time t1
            pure $ realToFrac (diffUTCTime t1 t0)

          let oldGameEvents = stateUnhandledEvents s
          newGameEvents <- mconcat <$> traverse translateGameEvents evs
          let evs = newGameEvents <> oldGameEvents
          let (newGameState, newGameEvents) =
                Game.processEvents evs (stateGame s)

          let drawCalls = generateDrawCalls textureAtlas newGameState
          executeDrawCalls ren drawCalls

          pure $ State { stateGame = newGameState
                       , stateUnhandledEvents = newGameEvents
                       }

translateGameEvents :: Event -> IO [GameEvent]
translateGameEvents EventQuit = exitSuccess
translateGameEvents (EventMouseMotion dat) = pure $
  let
    SDL.P (SDL.V2 x y) = SDL.mouseMotionEventPos dat
  in
    [PlayerMoved (fromIntegral x, fromIntegral y)]
translateGameEvents (EventKey dat) = pure $
  case SDL.keysymKeycode (SDL.keyboardEventKeysym dat) of
    SDL.KeycodeR -> [Restart]
    _            -> []
translateGameEvents _ = pure []
