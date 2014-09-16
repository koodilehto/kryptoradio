{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromShow,fromChar)
import Control.Monad (unless)
import Control.Monad.STM
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.String (fromString)
import Network.HTTP.Types (ok200,badRequest400)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Console.CmdArgs.Implicit hiding (name)
import System.IO

import Resources
import Serial
import Serialization
import SyncTimer

data Args = Args { device :: String
                 , baud   :: Int
                 , host   :: String
                 , port   :: Int
                 } deriving (Show, Data, Typeable)

synopsis = Args { device = def &= argPos 0 &= typ "DEVICE"
                , baud = 0 &= help "Baud rate on serial port (default: do not set)"
                , host = "*" &= help "IP address to bind to (default: all)"
                , port = 3000 &= help "HTTP port to listen to (default: 3000)"
                }
           &= program "kryptoradio-broadcaster"
           &= summary "Kryptoradio Broadcaster v0.0.1"
           &= help "Listens to given HTTP port and sends commands to \
                   \serial port connected to DVB encoder."

main = do
  Args{..} <- cmdArgs synopsis
  let set = setHost (fromString host) $
            setPort port $
            defaultSettings
  res <- newResources resources
  timer <- newSyncTimer
  serial <- openSerialRaw device baud
  forkIO $ serializator timer (priorityTake res) serial
  putStrLn $ "Binding to " ++ show (getHost set) ++ ", port " ++ show (getPort set)
  runSettings set $ app res timer

app :: [Resource] -> SyncAct -> Application
app res timer req respond = case (requestMethod req,pathResource $ pathInfo req,pathInfo req) of
  ("GET",Just name,_) -> out ok200 $ describe name
  ("GET",_,[])        -> out ok200 $ describeAll res
  ("GET",_,[sync])    -> do
    i <- atomically timer
    atomically $ waitSync timer i
    out ok200 "Sync\n"
  ("PUT",Just r,_) -> do
    -- Put message in a queue
    delivery <- newTVarIO Waiting
    packet <- lazyRequestBody req
    atomically $ do
      old <- tryTakeTMVar (var r)
      -- If something is queued, report that we threw it away
      case old of
        Just (oldState,_) -> writeTVar oldState Replaced
        Nothing -> return ()
      putTMVar (var r) (delivery,packet)
    respond $ responseStream ok200 headers $ loop delivery Start
  ("DELETE",Just r,_) -> do
    -- Delete already queued message.
    deleted <- atomically $ do
      old <- tryTakeTMVar (var r)
      case old of
        Just (oldState,_) -> do
          writeTVar oldState Replaced
          return True
        Nothing -> return False
    out ok200 $ if deleted then "Ok\n" else "Empty\n"
  _ -> out badRequest400 "Invalid request\n"
  where findResource = flip lookup $ map (\x -> (name x,x)) res
        pathResource [name] = findResource name
        pathResource _ = Nothing
        headers = [("Content-Type", "text/plain")]
        out code text = respond $ responseLBS code headers text
        loop var old write flush = do
          new <- atomically $ do
            x <- readTVar var
            if (x==old) then retry else return x
          write $ fromShow new
          write $ fromChar '\n'
          flush
          case new of
            Replaced -> return ()
            Sent -> return ()
            _ -> loop var new write flush
