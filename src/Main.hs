{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (void)
import System.Exit (exitSuccess)
import qualified SDL.Raw as SDLRaw

import Gal.App (App)
import Gal.Render (withRenderer, executeDrawCalls, generateDrawCalls)
import Gal.Texture (loadTextureAtlas)
import Gal.Time (mkTime, getDeltaTime)
import Gal.Window.SDL ( withSDLWindow, withWindowEvents )
import qualified Gal.App as App

main :: IO ()
main = do
  let initialWidth = 800
      initialHeight = 600
  withSDLWindow initialWidth initialHeight "gal" $ \win -> do
    _ <- SDLRaw.showCursor 0
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
