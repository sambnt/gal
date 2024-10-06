module Gal.Event ( Event(..)
                 ) where

import qualified SDL

data Event = EventQuit
           | EventKey SDL.KeyboardEventData
           | EventMouseButton SDL.MouseButtonEventData
           | EventMouseMotion SDL.MouseMotionEventData
           | EventMouseWheel SDL.MouseWheelEventData
           | EventTextInput SDL.TextInputEventData
           deriving (Eq, Show)
