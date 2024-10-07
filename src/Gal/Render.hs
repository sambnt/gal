module Gal.Render where

import qualified SDL
import Control.Exception.Safe (bracket, MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Gal.Window.SDL (WindowSDL, getSDLWindow)
import Foreign.C (CInt)
import Data.Foldable (forM_)
import Gal.Game (Game (player, coins), Player, playerX, playerY, playerWidth, playerHeight, coinX, coinY, coinWidth, coinHeight)
import Data.Functor ((<&>))
import Gal.Texture (TextureAtlas, textureRaven, textureCoin)

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

data DrawCall = DrawCall { texture :: SDL.Texture
                         , drawTo  :: SDL.Rectangle CInt
                         }
  deriving Eq

generateDrawCalls :: TextureAtlas -> Game -> [DrawCall]
generateDrawCalls atlas game =
  let
    p = player game
    playerDrawCall = DrawCall { texture = textureRaven atlas
                              , drawTo = SDL.Rectangle
                                  (SDL.P $ SDL.V2 (playerX p)
                                                  (playerY p)
                                  )
                                  (SDL.V2 (playerWidth p)
                                          (playerHeight p)
                                  )
                              }
    coinDrawCalls = coins game <&> \coin ->
      DrawCall { texture = textureCoin atlas
               , drawTo = SDL.Rectangle
                   (SDL.P $ SDL.V2 (coinX coin)
                                   (coinY coin)
                   )
                   (SDL.V2 (coinWidth coin)
                           (coinHeight coin)
                   )
               }
  in
    coinDrawCalls <> [playerDrawCall]

executeDrawCalls :: SDL.Renderer -> [DrawCall] -> IO ()
executeDrawCalls ren ds = do
  SDL.clear ren
  forM_ ds $ \d ->
    SDL.copy ren (texture d) Nothing (Just $ drawTo d)
  SDL.present ren
