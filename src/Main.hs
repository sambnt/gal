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
import Gal.Render (withRenderer)
import qualified SDL
import qualified SDL.Raw as SDLRaw
import qualified Control.Concurrent.STM as STM
import Data.Ratio ((%))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Foreign.C (CInt)

data Player = Player { playerPosition :: SDL.V2 Rational
                     , playerWidth    :: Natural
                     , playerHeight   :: Natural
                     }
  deriving (Eq, Ord, Show)

data Coin = Coin { coinPosition :: SDL.V2 CInt
                 , coinWidth :: Natural
                 , coinHeight :: Natural
                 }
  deriving (Eq, Ord, Show)

data State = State { statePlayer :: Player
                   , stateCoins :: [Coin]
                   }
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  let
    initialWidth = 800
    initialHeight = 600
    initialState = State { statePlayer = Player
                             { playerPosition = SDL.V2 (fromIntegral initialWidth % 2)
                                                       (fromIntegral initialHeight % 2)
                             , playerWidth = 32
                             , playerHeight = 32
                             }
                         , stateCoins = [ Coin { coinPosition = SDL.V2 100 200
                                               , coinWidth = 32
                                               , coinHeight = 32
                                               }
                                        ]
                         }
  withSDLWindow initialWidth initialHeight "gal" $ \win -> do
    SDLRaw.showCursor 0
    withRenderer win $ \ren -> do
      -- Load image
      let imageFileName = "./img/raven.bmp"
      putStrLn ("Loading " ++ imageFileName)
      bmp <- SDL.loadBMP imageFileName
      -- Create a texture from the surface
      tex <- SDL.createTextureFromSurface ren bmp

      time <- STM.newTVarIO =<< getCurrentTime

      let
        loop :: State -> (State -> IO State) -> IO State
        loop s0 f = do
            s1 <- f s0
            loop s1 f
      void $ loop initialState $ \s -> do
        start <- SDL.ticks
        withWindowEvents win $ \evs -> do
          when (not $ null evs) $
            print evs
          t1 <- getCurrentTime
          dt <- STM.atomically $ do
            t0 <- STM.readTVar time
            STM.writeTVar time t1
            pure $ realToFrac (diffUTCTime t1 t0)

          s' <- foldM (processEvent dt) s evs

          let player = statePlayer s
          SDL.clear ren
          let
            width = playerWidth player
            height = playerHeight player
            point = SDL.P $ round <$> playerPosition player
            size  = SDL.V2 (fromIntegral $ playerWidth player)
                           (fromIntegral $ playerHeight player)
            dest = SDL.Rectangle point size

          SDL.copy ren tex Nothing (Just dest)
          SDL.present ren

          -- frameTime <- ((-) start) <$> SDL.ticks
          -- let fps = if frameTime > 0 then 1000.0 / fromIntegral frameTime else 0.0;
          -- print fps
          pure s'

processEvent :: Float -> State -> Event -> IO State
processEvent _ _ EventQuit = exitSuccess
processEvent ds s (EventMouseMotion dat) =
  let
    SDL.P pos = SDL.mouseMotionEventPos dat
  in
    pure $
    s { statePlayer = (statePlayer s)
                        { playerPosition = fromIntegral <$> pos }
      }
processEvent dt s (EventKey dat) =
  pure $ case SDL.keysymKeycode (SDL.keyboardEventKeysym dat) of
    SDL.KeycodeUp ->
      let
        (SDL.V2 prevX prevY) = playerPosition $ statePlayer s
        newPlayerPosition = SDL.V2 prevX (prevY - (100000.0 * realToFrac dt))
      in
        s { statePlayer = (statePlayer s)
                          { playerPosition = newPlayerPosition
                          }
          }
    SDL.KeycodeDown ->
      let
        (SDL.V2 prevX prevY) = playerPosition $ statePlayer s
        newPlayerPosition = SDL.V2 prevX (prevY + (100000.0 * realToFrac dt))
      in
        s { statePlayer = (statePlayer s)
                          { playerPosition = newPlayerPosition
                          }
          }
    _ -> s
processEvent _ s _ = pure s
