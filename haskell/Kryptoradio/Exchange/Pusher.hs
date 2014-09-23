{-# LANGUAGE OverloadedStrings #-}
-- |Handles Websocket connection to Pusher and parses its messages
module Kryptoradio.Exchange.Pusher (Pusher(..),connectPusher) where

import Control.Applicative
import Control.Concurrent.STM.TChan
import Control.Monad (forever,mzero)
import Control.Monad.STM
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.WebSockets (ClientApp,Connection,sendTextData,runClient,receiveData)
import Kryptoradio.Exchange.Retry

data Pusher = Pusher { event   :: Text
                     , payload :: Value
                     , channel :: Text
                     } deriving (Show)

type Conv a = (Pusher -> Maybe a)

instance FromJSON Pusher where
  parseJSON (Object v) = Pusher <$>
                         v .: "event" <*>
                         (v .: "data" >>= nestedJson) <*>
                         v .:? "channel" .!= ""
  parseJSON _ = mzero

-- |Parser for nested JSON. That is a JSON encoded value in a JSON
-- string. Pusher likes them, we don't, but here's a parser for them.
nestedJson :: (FromJSON a) => Text -> Parser a
nestedJson = either fail return . eitherDecodeStrict' . encodeUtf8

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

connectPusher :: String -> Int -> String -> [Text] -> Conv a -> TChan a -> IO ()
connectPusher host port appKey chans f chan = foreverRetry worker printException
  where subs = map channelToSubs chans
        path = "/app/" ++ appKey ++ "?protocol=7"
        worker = runClient host port path $ app chan subs f

channelToSubs :: Text -> ByteString
channelToSubs chan =
  encode $ object ["event" .= String "pusher:subscribe"
                  ,"data"  .= object ["channel" .= chan]
                  ]
