module Gal.App where

import Gal.Game.Types (Game, GameEvent)
import qualified Gal.Game as Game
import Gal.Time (DeltaTimeSec)
import Gal.Event (Event)
import Gal.Input (translateGameEvents)

import System.Exit (exitSuccess)
import qualified Debug.Trace as Debug

data App = App { appGame :: Game
               , appUnhandledEvents :: [GameEvent]
               }

init :: App
init = App { appGame = Game.initialGameState
           , appUnhandledEvents = []
           }

step :: DeltaTimeSec -> [Event] -> App -> Maybe App
step _dt evs app = do
  newGameEvents <- mconcat <$> traverse translateGameEvents evs
  let
    evsToProcess = newGameEvents <> appUnhandledEvents app
    (nextGameState, nextGameEvents) =
      Game.processEvents evsToProcess (appGame app)

  pure $ App { appGame = nextGameState
             , appUnhandledEvents = nextGameEvents
             }
