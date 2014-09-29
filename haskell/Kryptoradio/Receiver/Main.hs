{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (readTChan,newTChanIO)
import System.IO (stdout) -- temp
import Kryptoradio.Receiver.Dvb
import Kryptoradio.Receiver.Parser
import Kryptoradio.Receiver.Resources

main = do
  putStrLn "It compiles! Hard coded tuning to 634 MHz, listening to PID 8101"
  (h,dvb) <- openDvb 0 0 0 634000000 8101
  resVar <- newTVarIO []
  -- FIXME if using threaded runtime the handle has extremely high
  -- latency (minutes) when run inside forkIO
  forkIO $ krpToChan h resVar
  putStrLn "Waiting sync"
  res <- atomically $ do
    res <- readTVar resVar
    if (null res) then retry else return res
  putStrLn $ "Got first sync. Resources: " ++ (show $ map resourceToText res)
  let broadcastChan = var (res !! 2)
  chan <- atomically $ dupTChan broadcastChan
  forever $ do
    m <- atomically $ readTChan chan
    B.hPut stdout m
  closeDvb dvb

resourceToText Resource{..} = (rid,name,desc)
