module Gal.Input where

import qualified SDL

import Gal.Window.Event (Event (EventQuit, EventMouseMotion, EventKey))
import Gal.Game.Types (GameEvent (Restart, PlayerMoved))

translateGameEvents :: Event -> Maybe [GameEvent]
translateGameEvents EventQuit = Nothing
translateGameEvents (EventMouseMotion dat) = Just $
  let
    SDL.P (SDL.V2 x y) = SDL.mouseMotionEventPos dat
  in
    [PlayerMoved (fromIntegral x, fromIntegral y)]
translateGameEvents (EventKey dat) = Just $
  case SDL.keysymKeycode (SDL.keyboardEventKeysym dat) of
    SDL.KeycodeR -> [Restart]
    _            -> []
translateGameEvents _ = Just []
