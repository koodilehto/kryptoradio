{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (readTChan,newTChanIO)
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
  let loop old = do
        new <- atomically $ do
          res <- readTVar resVar
          let new = map resourceToText res
          if (new == old) then retry else return new
        print new
        loop new
  loop [""]
  closeDvb dvb

resourceToText Resource{..} = show (rid,name,desc)
