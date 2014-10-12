{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Main where

import Data.Functor
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (finally)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import System.Console.CmdArgs.Implicit hiding (name)
import System.Directory
import System.IO

import Resources
import Serial
import MockSerial
import Serialization
import SyncTimer
import SimpleSockets

data Args = Args { device :: String
                 , baud   :: Int
                 , socket :: FilePath
                 , mock   :: Bool
                 } deriving (Show, Data, Typeable)

synopsis defSockPath =
  Args { device = def &= argPos 0 &= typ "DEVICE"
       , baud = 0 &= help "Baud rate on serial port (required if not in mock mode)"
       , socket = defSockPath &= typFile &=
                  help ("UNIX domain socket to listen. Default: " ++ defSockPath)
       , mock = False &= help "Use file or named pipe instead of \
                              \real serial hardware. If baud rate is \
                              \set, it emulates limited throughput, \
                              \too."
       }
  &= program "kryptoradio-encoder"
  &= summary "Kryptoradio Encoder v0.0.1"
  &= help "Listens to given HTTP port and sends commands to \
          \serial port connected to DVB encoder."

main = do
  defSockPath <- getAppUserDataDirectory "kryptoradio-encoder"
  Args{..} <- cmdArgs $ synopsis defSockPath
  when (not mock && baud == 0) $ error "Baud rate is not set. See --help"
  res <- newResources resources
  timer <- newSyncTimer
  (serialClose,writeSerial) <- if mock
                               then openMockSerialOut device baud
                               else openSerialOutRaw device baud
  -- Prepare socket dir
  putStrLn $ "Listening to " ++ socket
  createDirectoryIfMissing False socket
  threads <- flip mapM res $ \r ->
    foreverAccept (app r timer) $ socket ++ "/" ++ T.unpack (name r)
  -- Start serial traffic handler in main thread. On shutdown, give
  -- 100ms for thread clean-up.
  finally
    (serializator timer (priorityTake res) writeSerial (syncPacket res))
    (mapM_ killThread threads >> threadDelay 100000)

-- |Main app for handling incoming connection to a handle.
app :: Resource -> SyncAct -> Handle -> IO ()
app res timer h = do
  hSetBuffering h NoBuffering
  forkIO $ foreverSync timer $ B.hPut h "S"
  forever $ handleMsg res h

-- |Handle single message. Returns after delivery or deletion.
handleMsg :: Resource -> Handle -> IO ()
handleMsg r h = do
  -- Take given number of bytes
  len <- hGetLine h
  packet <- case readMaybe (removeD len) of
    Just n -> B.hGet h n
    _ -> do
      B.hPut h "E"
      fail "Not an integer"

  -- Write it to underlying worker
  delivery <- newTVarIO Waiting
  atomically $ putTMVar (var r) (delivery,packet)

  -- Wait for delivery
  cmdVar <- newEmptyTMVarIO
  thread <- forkIO $ B.hGet h 1 >>= atomically . putTMVar cmdVar
  delivered <- atomically $ do
    cmd <- tryTakeTMVar cmdVar
    del <- tryReadTMVar $ var r
    case (cmd,del) of
      (_,Nothing) -> return True
      (Just "D",_) -> do
        takeTMVar $ var r
        return False
      _ -> retry
  killThread thread

  -- Take action
  B.hPut h $ if delivered then "R" else "D"

-- |Like `read` but returns Nothing instead of an error if parsing
-- fails. Is here becauses function `readMaybe` was introduced in
-- base-4.6 which is not yet mainstream.
readMaybe x = case readsPrec 0 x of
  [(a,"")] -> Just a
  _        -> Nothing

-- |Strips D from beginning. Sometimes data feeder is too slow and
-- does send D after it has read R from input. This is not fatal but
-- messes integer parser without this.
removeD :: String -> String
removeD ('D':xs) = xs
removeD xs = xs
