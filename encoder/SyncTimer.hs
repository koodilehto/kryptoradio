-- |Module for sync timing.
module SyncTimer where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar

type SyncAct = STM Integer

syncInterval :: Int
syncInterval = 60 -- seconds

-- |Create sync timer. There needs to be only one instance wide timer.
newSyncTimer :: IO SyncAct
newSyncTimer = do
  var <- newTVarIO 1
  forkIO $ forever $ do
    threadDelay $ syncInterval*10^6
    atomically $ modifyTVar' var succ
  return $ readTVar var

-- |Wait for a sync. This blocks until sync number `start` occurs. More
-- precisely, when the transmitter is also starting the sync. The time
-- of sync may be earlier than when it actually goes to the air.
waitSync :: SyncAct -> Integer -> SyncAct
waitSync act start = do
  now <- act
  if now <= start then retry else return now

-- |Run given IO action forever on every sync (until exception occurs)
foreverSync :: SyncAct -> IO a -> IO ()
foreverSync act io = do
  atomically act >>= foreverSync'
  where foreverSync' i = do
          new <- atomically $ waitSync act i
          io
          foreverSync' new
