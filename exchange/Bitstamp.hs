{-# LANGUAGE OverloadedStrings #-}
module Bitstamp where

import Control.Applicative
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM.TChan
import Control.Exception.Base (SomeException,throw)
import Control.Monad (forever, mzero)
import Control.Monad.STM
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.WebSockets (ClientApp,Connection,sendTextData,runClient,receiveData)

data Pusher = Pusher { event   :: Text
                     , payload :: Value
                     , channel :: Text
                     } deriving (Show)

instance FromJSON Pusher where
  parseJSON (Object v) = Pusher <$>
                         v .: "event" <*>
                         (v .: "data" >>= either fail return . eitherDecode') <*>
                         v .:? "channel" .!= ""
  parseJSON _ = mzero

join :: Connection -> IO ()
join conn = mapM_ (sendTextData conn) m
  where
    m :: [ByteString]
    m = ["{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"diff_order_book\"}}"
        ,"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"live_trades\"}}"
        ]

app :: TChan Pusher -> ClientApp ()
app chan conn = do
  putStrLn "Connected!"
  join conn
  -- Fork a thread that writes WS data to stdout
  forever $ do
    msg <- receiveData conn
    atomically $ writeTChan chan $ case eitherDecode' msg of
      Left e -> error e
      Right a -> a

-- |Pass the grenade to the channel
rethrow :: TChan a -> Either SomeException b -> IO ()
rethrow ch x = atomically $ writeTChan ch $ case x of
  Right _ -> error "Web socket closed"
  Left e  -> throw e

main :: IO ()
main = do
  chan <- newTChanIO
  forkFinally (runClient "ws.pusherapp.com" 80 "/app/de504dc5763aeef9ff52?protocol=7" $ app chan) (rethrow chan)
  forever $ do
    x <- atomically $ readTChan chan
    print x
