{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Main where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8 (fromString)
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (readTChan,newTChanIO)
import Data.Aeson
import qualified Data.String as S
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Monoid
import Data.Version
import Data.Word
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import System.Console.CmdArgs.Implicit hiding (name)
import Text.CSV
import Text.Printf

import Dvb
import Parser
import Resources
import Paths_kryptoradio_receiver

data Args = Args { device   :: Int
                 , frontend :: Int
                 , demuxer  :: Int
                 , freq     :: Int
                 , pid      :: Int
                 , host     :: String
                 , port     :: Int
                 , static   :: Maybe String
                 } deriving (Show, Data, Typeable)

synopsis =
  Args { device = 0 &= help "DVB device id (default: 0)"
       , frontend = 0 &= help "DVB frontend id (default: 0)"
       , demuxer = 0 &= help "DVB demuxer id (default: 0)"
       , freq = 0 &= argPos 0 &= typ "HERTZ"
       , pid = 8101 &= help "Kryptoradio PID (default: 8101)"
       , host = "*" &= help "IP address to bind to (default: all)"
       , port = 3000 &= help "HTTP port to listen to (default: 3000)"
       , static = def &= typDir &= help "Path to static WWW directory"
       }
  &= program "kryptoradio-receiver"
  &= summary ("Kryptoradio Receiver " ++ showVersion version)
  &= help "Listens to given HTTP port for connections while \
          \receiving and decoding data from DVB device connected \
          \to the system. If you have only one ordinary DVB-adapter \
          \in your system, you don't need to set device, frontend \
          \nor demuxer IDs. If you are receiving from Digita \
          \broadcast in Finland, the default PID is also fine. \
          \Frequency must be given in Hz. If you want to host local files \
          \in addition to the API, provide static WWW directory."

main = do
  -- Figure out settings
  Args{..} <- cmdArgs synopsis
  let set = setHost (S.fromString host) $
            setPort port $
            defaultSettings
  let staticApp = case static of
        Nothing  -> id -- No static hosting
        Just dir -> (staticPolicy $ addBase dir) .
                    (staticPolicy $ addBase dir <> addSuffix "/index.html") .
                    (staticPolicy $ addBase dir <> addSuffix ".html")

  -- Debug messages
  putStrLn $ "Tuning to " ++ show (fromIntegral freq / 1e6) ++ "MHz, PID " ++ show pid
  putStrLn $ "Binding to " ++ show (getHost set) ++ ", port " ++ show (getPort set)
  case static of
    Just dir -> putStrLn $ "Hosting static WWW directory at " ++ dir
    Nothing  -> return ()

  -- Tune in and start processing of messages
  (h,dvb) <- openDvb device frontend demuxer freq pid
  resVar <- newTVarIO []
  -- FIXME if using threaded runtime the handle has extremely high
  -- latency (minutes) when run inside forkIO
  forkIO $ krpToChan h resVar

  -- Start web service
  runSettings set $ staticApp $ api resVar
  closeDvb dvb

-- |Add given suffix to every URL.
addSuffix :: String -> Policy
addSuffix suffix = policy $ \base -> Just (base ++ suffix)

api :: TVar [Resource] -> Application
api var req respond = do
  resources <- readTVarIO var
  let byName = flip lookup $ map (\Resource{..} -> (name,var)) resources
  case (requestMethod req,pathInfo req) of
    ("GET",["api"]) ->
      respond $ jsonData ok200 $
      object ["name" .= ("Kryptoradio DVB-T receiver" :: Text)
             ,"synced" .= not(null resources)
             ]
    ("GET",["api","waitsync"]) -> do
      atomically $ do
        x <- readTVar var
        when (null x) retry
      respond $ jsonData ok200 True
    ("GET",["api","resources"]) ->
      respond $ jsonData ok200 $ map resourceToValue resources
    ("GET",["api","resource",res,fmt]) -> do
      case byName res of
        Nothing -> respond $ jsonData notFound404 $
                   object ["error" .= ("Resource not found" :: Text)]
        Just bchan -> do
          chan <- atomically $ dupTChan bchan
          respond $ case fmt of
            "raw" -> rawStream chan
            "json" -> jsonStream chan
            "jsoncsv" -> jsonCsvStream chan
            _ -> jsonData badRequest400 $
                 object ["error" .= ("Unknown format" :: Text)]

-- |Outputs binary data stream. Data consists of variable-sized
-- chunks, in which the first 4 bytes contain chunk length in big
-- endian format and it is followed by the data of that chunk.
rawStream :: TChan B.ByteString -> Response
rawStream chan = responseStream status200 rawHeader $ \write flush -> forever $ do
  x <- atomically $ readTChan chan
  write $ fromWord32be $ fromIntegral $ B.length x
  write $ fromLazyByteString x
  flush

-- |Output JSON stream of (8-bit string data in quotes, properly
-- escaped). Useful for Streaming JSON loading in JavaScript.
jsonStream :: TChan B.ByteString -> Response
jsonStream chan = responseStream status200 jsonHeader $ \write flush -> forever $ do
  write $ fromByteString "[\""
  flush
  forever $ do
    x <- atomically $ readTChan chan
    write $ B.foldl escape mempty x <> quote
    flush
  where quote = fromByteString "\",\""

-- |Output JSON stream of values encoded in CSV. Only some resources
-- use CSV formatting so this may result weird results if used with
-- binary data. Input is assumed to be encoded in UTF-8.
jsonCsvStream :: TChan B.ByteString -> Response
jsonCsvStream chan = responseStream status200 jsonHeader $ \write flush -> forever $ do
  write $ fromByteString "["
  flush
  forever $ do
    x <- atomically $ readTChan chan
    write $ fromLazyByteString $ case parseCSV "" $ T.unpack $ T.decodeUtf8 x of
      Left e -> encode $ object ["error" .= ("Unable to parse: " ++ show e)]
      Right x -> encode x
    write $ fromByteString ","
    flush

-- |Escapes given byte using JSON escaping rules (which are a bit
-- ambiguous but I assume they're same as in ASCII). This is done
-- manually instead of aeson library because they have dropped
-- ByteString serialization support.
escape :: Builder -> Word8 -> Builder
escape acc byte | byte < 32 || byte > 126 = acc <> (fromString $ printf "\\u%04x" byte)
                | otherwise               = acc <> (fromByteString $ BS.singleton byte)

resourceToValue Resource{..} = object ["rid" .= rid, "name" .= name, "desc" .= desc]

jsonData code = responseLBS code jsonHeader . encode

rawHeader = [("Content-Type", "application/octet-stream")]
jsonHeader = [("Content-Type", "application/json")]
