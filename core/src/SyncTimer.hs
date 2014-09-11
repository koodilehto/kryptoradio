-- |Module for sync timing.
module SyncTimer where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Data.Time.Clock.POSIX

type SyncVar = TVar POSIXTime

syncInterval :: POSIXTime
syncInterval = 60 -- Sync every 60 seconds

-- |Create sync timer. There needs to be only one instance wide timer.
newSyncTimer :: IO SyncVar
newSyncTimer = newTVarIO 0

-- |Reset timer if there is certain amount of time since last
-- reset. Return True if it's time for a sync. It is intentional
-- feature that timer is not reset periodically but when this function
-- is called.
maybeReset :: SyncVar -> IO Bool
maybeReset var = do
  trigTime <- readTVarIO var
  now <- getPOSIXTime
  if trigTime > now
    then return False
    else do atomically $ writeTVar var $ now + syncInterval
            return True

-- |Wait for a sync. This is not the time sync will happen but it is
-- the time that sync will be emitted when something is written.
waitSync :: SyncVar -> IO ()
waitSync var = do
  trigTime <- readTVarIO var
  now <- getPOSIXTime
  when (trigTime > now) $ threadDelay $ ceiling $ 10^6 * (trigTime-now)
