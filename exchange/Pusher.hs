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

type Conv a = (Pusher -> Maybe a)

instance FromJSON Pusher where
  parseJSON (Object v) = Pusher <$>
                         v .: "event" <*>
                         (v .: "data" >>= either fail return . eitherDecode') <*>
                         v .:? "channel" .!= ""
  parseJSON _ = mzero

app :: TChan a -> [ByteString] -> Conv a -> ClientApp ()
app chan subs f conn = do
  -- Subscribe to Pusher channels
  mapM_ (sendTextData conn) subs
  -- Do actions
  forever $ do
    msg <- receiveData conn
    case eitherDecode' msg of
      Left e  -> atomically $ writeTChan chan $ error e
      Right a -> case f a of
        Nothing -> return ()
        Just x  -> atomically $ writeTChan chan x

-- |Pass the grenade to the channel
rethrow :: TChan a -> Either SomeException b -> IO ()
rethrow ch x = atomically $ writeTChan ch $ case x of
  Right _ -> error "Web socket closed"
  Left e  -> throw e

connectPusher :: String -> Int -> String -> [Text] -> Conv a -> IO (TChan a)
connectPusher host port appKey chans f = do
  chan <- newTChanIO
  forkFinally
    (runClient host port path $ app chan subs f)
    (rethrow chan)
  return chan
  where subs = map channelToSubs chans
        path = "/app/" ++ appKey ++ "?protocol=7"

channelToSubs :: Text -> ByteString
channelToSubs chan =
  encode $ object ["event" .= String "pusher:subscribe"
                  ,"data"  .= object ["channel" .= chan]
                  ]
