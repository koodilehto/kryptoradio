{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromShow,fromChar)
import Control.Monad (when)
import Control.Monad.STM
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Exception (SomeException)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.String (fromString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Console.CmdArgs.Implicit hiding (name)
import System.IO

import Kryptoradio.Core.Resources
import Kryptoradio.Core.Serial
import Kryptoradio.Core.MockSerial
import Kryptoradio.Core.Serialization
import Kryptoradio.Core.SyncTimer

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
  failVar <- newTVarIO Nothing
  (serialClose,writeSerial) <- if mock
                               then openMockSerialOut device baud
                               else openSerialOutRaw device baud
  forkFinally (serializator timer (priorityTake res) writeSerial)
    (storeError failVar)
  putStrLn $ "Binding to " ++ show (getHost set) ++ ", port " ++ show (getPort set)
  runSettings set $ app res timer failVar

app :: [Resource] -> SyncAct -> TVar (Maybe String) -> Application
app res timer failVar req respond = do
  hasFailed <- readTVarIO failVar
  case (hasFailed,requestMethod req,pathResource $ pathInfo req,pathInfo req) of
    (Just e,_,_,_) -> respond $ text serviceUnavailable503 $
                      "Worker has died: " `B.append` (B.pack e)
    (_,"GET",Just name,_) -> respond $ text ok200 $ describe name
    (_,"GET",_,[])        -> respond $ text ok200 $ describeAll res
    (_,"GET",_,["sync"])    -> do
      i <- atomically timer
      atomically $ waitSync timer i
      respond $ text ok200 "Sync"
    (_,"PUT",Just r,_) -> do
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
          -- Deciding delivery log style. Either simple which just
          -- says if it's queued or replaced by using response code,
          -- or full which reports all intermediate steps, including
          -- final delivery.
          if isFullDelivery
            then respond $ responseStream ok200 plainText $
                 fullDelivery delivery Start
            else simpleDelivery delivery >>= respond
        Nothing -> respond $ text badRequest400 "Invalid delivery option"
    (_,"DELETE",Just r,_) -> do
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

storeError :: TVar (Maybe String) -> Either SomeException a -> IO ()
storeError var out = case out of
  Left e -> do
    atomically $ writeTVar var $ Just $ show e
    hPutStrLn stderr $ show e
  _ -> atomically $ writeTVar var $ Just "Worker has stopped"
