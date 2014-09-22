{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- |Connects and parses Bitstamp exchange feed
module Kryptoradio.Exchange.Bitstamp (bitstamp) where

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM.TChan
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific (Scientific,fromFloatDigits)
import Kryptoradio.Exchange.Pusher
import Kryptoradio.Exchange.Exchange

-- |Subscriptions to Bitstamp live order book and trade stream
subs :: [Text]
subs = ["diff_order_book","live_trades"]

extract Pusher{..} = case (event,channel) of
  ("data","diff_order_book") -> either fail Just $ parseEither orderParser payload
  ("trade","live_trades") -> either fail Just $ parseEither tradeParser payload
  _ -> Nothing
  where
    -- Dig all bids and asks and capsule them again
    orderParser (Object o) = do
      bids <- map (conv Bid) <$> o .: "bids"
      asks <- map (conv Ask) <$> o .: "asks"
      return $ bids++asks
    orderParser _ = mzero
    -- Trades happen once at a time. Take it out and put inside singleton list
    tradeParser (Object o) = do
      price <- fixScifi <$> o .: "price"
      amount <- fixScifi <$> o .: "amount"
      return [(Key Trade price "USD" "XBT" "BITSTAMP",amount)]
    conv entry (price,amount) = (Key entry (read price) "USD" "XBT" "BITSTAMP",read amount)

bitstamp :: TChan [Entry] -> IO ()
bitstamp = connectPusher "ws.pusherapp.com" 80 "de504dc5763aeef9ff52" subs extract

-- |This fixes issue with Bitstamp trade data. When read as double and
-- then converted to Scientific, automatic error rounding takes
-- place. Somewhat twisted, but works with current data.
fixScifi :: Double -> Scientific
fixScifi = fromFloatDigits
