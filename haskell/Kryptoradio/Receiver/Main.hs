module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan,newTChanIO)
import Kryptoradio.Receiver.Dvb
import Kryptoradio.Receiver.Parser

main = do
  putStrLn "It compiles! Hard coded tuning to 634 MHz, listening to PID 8101"
  (h,dvb) <- openDvb 0 0 0 634000000 8101
  chan <- newTChanIO
  -- FIXME if using threaded runtime the handle has extremely high
  -- latency (minutes) when run inside forkIO
  forkIO $ pesToChan h chan
  forever $ do
    putStr "Kryptoradio packet: "
    bs <- atomically $ readTChan chan
    print bs
  closeDvb dvb
