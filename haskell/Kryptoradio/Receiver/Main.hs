{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Main where

import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Word
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (readTChan,newTChanIO)
import Data.Aeson
import Data.String (fromString)
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Console.CmdArgs.Implicit hiding (name)
import System.IO (stdout) -- temp
import Kryptoradio.Receiver.Dvb
import Kryptoradio.Receiver.Parser
import Kryptoradio.Receiver.Resources

data Args = Args { device   :: Int
                 , frontend :: Int
                 , demuxer  :: Int
                 , freq     :: Int
                 , pid      :: Int
                 , host     :: String
                 , port     :: Int
                 } deriving (Show, Data, Typeable)

synopsis = Args { device = 0 &= help "DVB device id (default: 0)"
                , frontend = 0 &= help "DVB frontend id (default: 0)"
                , demuxer = 0 &= help "DVB demuxer id (default: 0)"
                , freq = 0 &= argPos 0 &= typ "HERTZ"
                , pid = 8101 &= help "Kryptoradio PID (default: 8101)"
                , host = "*" &= help "IP address to bind to (default: all)"
                , port = 3000 &= help "HTTP port to listen to (default: 3000)"
                }
           &= program "kryptoradio-receiver"
           &= summary "Kryptoradio Receiver v0.0.1"
           &= help "Listens to given HTTP port for connections while \
                   \receiving and decoding data from DVB device connected \
                   \to the system. If you have only one ordinary DVB-adapter \
                   \in your system, you don't need to set device, frontend \
                   \nor demuxer IDs. If you are receiving from Digita \
                   \broadcast in Finland, the default PID is also fine. \
                   \Frequency must be given in Hz."

main = do
  Args{..} <- cmdArgs synopsis
  let set = setHost (fromString host) $
            setPort port $
            defaultSettings
  putStrLn $ "Tuning to " ++ show (fromIntegral freq / 1e6) ++ "MHz, PID " ++ show pid
  putStrLn $ "Binding to " ++ show (getHost set) ++ ", port " ++ show (getPort set)
  (h,dvb) <- openDvb device frontend demuxer freq pid
  resVar <- newTVarIO []
  -- FIXME if using threaded runtime the handle has extremely high
  -- latency (minutes) when run inside forkIO
  forkIO $ krpToChan h resVar
  -- Ready to serve
  runSettings set $ app resVar
  closeDvb dvb

app :: TVar [Resource] -> Application
app var req respond = do
  resources <- readTVarIO var
  let byName = flip lookup $ map (\Resource{..} -> (name,var)) resources
  case (requestMethod req,pathInfo req) of
    ("GET",["api"]) ->
      respond $ jsonData ok200 $
      object ["name" .= ("Kryptoradio DVB-T receiver" :: Text)
             ,"synced" .= not(null resources)
             ]
    ("GET",["api","resources"]) ->
      respond $ jsonData ok200 $ map resourceToValue resources
    ("GET",["api","resource",res,fmt]) -> do
      case byName res of
        Nothing -> respond $ jsonData notFound404 $
                   object ["error" .= ("Resource not found" :: Text)]
        Just bchan -> do
          chan <- atomically $ dupTChan bchan
          respond $ responseStream status200 rawHeader $ \write flush -> forever $ do
            x <- atomically $ readTChan chan
            write $ fromWord32be $ fromIntegral $ B.length x
            write $ fromLazyByteString x
            flush

resourceToValue Resource{..} = object ["rid" .= rid, "name" .= name, "desc" .= desc]

jsonData code = responseLBS code [("Content-Type", "application/json")] . encode

rawHeader = [("Content-Type", "application/octet-stream")]
