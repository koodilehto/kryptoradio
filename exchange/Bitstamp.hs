{-# LANGUAGE OverloadedStrings #-}
-- |Connects and parses Bitstamp exchange feed
module Bitstamp (bitstamp) where

import Control.Concurrent.STM.TChan
import Data.Text (Text)
import Pusher

-- |Subscriptions to Pusher channels
subs :: [Text]
subs = ["diff_order_book","live_trades"]

bitstamp :: IO (TChan Pusher)
bitstamp = connectPusher "ws.pusherapp.com" 80 "de504dc5763aeef9ff52" subs
