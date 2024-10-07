module Gal.Render where

import qualified SDL
import Control.Exception.Safe (bracket, MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Gal.Window.SDL (WindowSDL, getSDLWindow)

withRenderer
  :: (MonadMask m,  MonadIO m)
  => WindowSDL
  -> (SDL.Renderer -> m c)
  -> m c
withRenderer win f =
  let
    sdlRendererConfig =
      SDL.RendererConfig
      { SDL.rendererType          = SDL.AcceleratedRenderer -- No VSync
      , SDL.rendererTargetTexture = False
      }
  in
    bracket
      (SDL.createRenderer (getSDLWindow win) (-1) sdlRendererConfig)
      SDL.destroyRenderer
      f
