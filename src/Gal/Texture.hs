module Gal.Texture where

import qualified SDL

data TextureAtlas = TextureAtlas { textureRaven :: SDL.Texture
                                 , textureCoin :: SDL.Texture
                                 }

loadTextureAtlas :: SDL.Renderer -> IO TextureAtlas
loadTextureAtlas ren = do
  TextureAtlas <$> loadTexture ren "./img/raven.bmp"
               <*> loadTexture ren "./img/coin.bmp"

loadTexture :: SDL.Renderer -> String -> IO SDL.Texture
loadTexture ren fileName = do
  putStrLn ("Loading " ++ fileName)
  bmp <- SDL.loadBMP fileName
  -- Create a texture from the surface
  SDL.createTextureFromSurface ren bmp
