module Gal.Time where

import qualified Control.Concurrent.STM as STM
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)

newtype Time = Time (STM.TVar UTCTime)

type DeltaTimeSec = NominalDiffTime

mkTime :: IO Time
mkTime = do
  t0 <- getCurrentTime
  Time <$> STM.newTVarIO t0

getDeltaTime :: Time -> IO DeltaTimeSec
getDeltaTime (Time time) = do
  t1 <- getCurrentTime
  STM.atomically $ do
    t0 <- STM.readTVar time
    STM.writeTVar time t1
    pure $ realToFrac (diffUTCTime t1 t0)
