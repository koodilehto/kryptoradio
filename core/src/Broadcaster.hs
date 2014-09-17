{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromShow,fromChar)
import Control.Monad (when)
import Control.Monad.STM
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.String (fromString)
import Network.HTTP.Types (Status,Query,ok200,badRequest400,conflict409)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Console.CmdArgs.Implicit hiding (name)
import System.IO

import Resources
import Serial
import MockSerial
import Serialization
import SyncTimer

data Args = Args { device :: String
                 , baud   :: Int
                 , host   :: String
                 , port   :: Int
                 , mock   :: Bool
                 } deriving (Show, Data, Typeable)

synopsis = Args { device = def &= argPos 0 &= typ "DEVICE"
                , baud = 0 &= help "Baud rate on serial port (required if not in mock mode)"
                , host = "*" &= help "IP address to bind to (default: all)"
                , port = 3000 &= help "HTTP port to listen to (default: 3000)"
                , mock = False &= help "Use file or named pipe instead of \
                                       \real serial hardware. If baud rate is \
                                       \set, it emulates limited throughput, \
                                       \too."
                }
           &= program "kryptoradio-broadcaster"
           &= summary "Kryptoradio Broadcaster v0.0.1"
           &= help "Listens to given HTTP port and sends commands to \
                   \serial port connected to DVB encoder."

main = do
  Args{..} <- cmdArgs synopsis
  when (not mock && baud == 0) $ error "Baud rate is not set. See --help"
  let set = setHost (fromString host) $
            setPort port $
            defaultSettings
  res <- newResources resources
  timer <- newSyncTimer
  (serialClose,writeSerial) <- if mock
                               then openMockSerialOut device baud
                               else openSerialOutRaw device baud
  forkIO $ serializator timer (priorityTake res) writeSerial
  putStrLn $ "Binding to " ++ show (getHost set) ++ ", port " ++ show (getPort set)
  runSettings set $ app res timer

app :: [Resource] -> SyncAct -> Application
app res timer req respond = case (requestMethod req,pathResource $ pathInfo req,pathInfo req) of
  ("GET",Just name,_) -> respond $ text ok200 $ describe name
  ("GET",_,[])        -> respond $ text ok200 $ describeAll res
  ("GET",_,[sync])    -> do
    i <- atomically timer
    atomically $ waitSync timer i
    respond $ text ok200 "Sync"
  ("PUT",Just r,_) -> do
    let deliveryOk = case findQuery "delivery" of
          Just (Just "simple") -> Just False
          Nothing              -> Just False
          Just (Just "full")   -> Just True
          _                    -> Nothing
    case deliveryOk of
      Just isFullDelivery -> do
        -- All is fine, start sending
        delivery <- newTVarIO Waiting
        packet <- lazyRequestBody req
        atomically $ do
          old <- tryTakeTMVar (var r)
          -- If something is queued, report that we threw it away
          case old of
            Just (oldState,_) -> writeTVar oldState Replaced
            Nothing -> return ()
          putTMVar (var r) (delivery,packet)
        -- Deciding delivery log style. Either simple which just says if
        -- it's queued or replaced by using response code, or full which
        -- reports all intermediate steps, including final delivery.
        if isFullDelivery
           then respond $ responseStream ok200 plainText $
                fullDelivery delivery Start
           else simpleDelivery delivery >>= respond
      Nothing -> respond $ text badRequest400 "Invalid delivery option"
  ("DELETE",Just r,_) -> do
    -- Delete already queued message.
    deleted <- atomically $ do
      old <- tryTakeTMVar (var r)
      case old of
        Just (oldState,_) -> do
          writeTVar oldState Replaced
          return True
        Nothing -> return False
    respond $ if deleted
              then text ok200 "Ok"
              else text conflict409 "Empty"
  _ -> respond $ text badRequest400 "Invalid request"
  where findResource = flip lookup $ map (\x -> (name x,x)) res
        pathResource [name] = findResource name
        pathResource _ = Nothing
        findQuery :: BS.ByteString -> Maybe (Maybe BS.ByteString)
        findQuery = flip lookup $ queryString req
        simpleDelivery var = atomically $ do
          x <- readTVar var
          case x of
            Replaced -> return $ text conflict409 "Replaced"
            Sending  -> return $ text ok200 "Sending"
            Sent     -> return $ text ok200 "Sending" -- Not "Sent" by purpose
            _        -> retry
        fullDelivery var old write flush = do
          new <- atomically $ do
            x <- readTVar var
            if (x==old) then retry else return x
          write $ fromShow new
          write $ fromChar '\n'
          flush
          case new of
            Replaced -> return ()
            Sent -> return ()
            _ -> fullDelivery var new write flush

-- |HTTP plain text response with custom status code and message.
text :: Status -> ByteString -> Response
text code text = responseLBS code plainText $ text `B.append` "\n"

plainText = [("Content-Type", "text/plain")]
