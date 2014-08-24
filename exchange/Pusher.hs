{-# LANGUAGE OverloadedStrings #-}
-- |Handles Websocket connection to Pusher and parses its messages
module Pusher (Pusher(..),connectPusher) where

import Control.Applicative
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM.TChan
import Control.Exception.Base (SomeException,throw)
import Control.Monad (forever,mzero)
import Control.Monad.STM
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
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

app :: TChan Pusher -> [ByteString] -> ClientApp ()
app chan subs conn = do
  -- Subscribe to Pusher channels
  mapM_ (sendTextData conn) subs
  -- Do actions
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

connectPusher :: String -> Int -> String -> [Text] -> IO (TChan Pusher)
connectPusher host port appKey chans = do
  chan <- newTChanIO
  forkFinally
    (runClient host port path $ app chan subs)
    (rethrow chan)
  return chan
  where subs = map channelToSubs chans
        path = "/app/" ++ appKey ++ "?protocol=7"

channelToSubs :: Text -> ByteString
channelToSubs chan =
  encode $ object ["event" .= String "pusher:subscribe"
                  ,"data"  .= object ["channel" .= chan]
                  ]
