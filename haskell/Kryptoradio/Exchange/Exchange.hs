-- |Common functions between exchanges
module Kryptoradio.Exchange.Exchange where

import Data.Scientific

data Kind = Bid   -- ^ Buying order
          | Trade -- ^ Completed trade
          | Ask   -- ^ Selling order
          | Rate  -- ^ Rate is used instead of trade when not
                  -- broadcasting full order book
          deriving (Show,Ord,Eq)

data Key = Key { kind     :: Kind   -- ^ Record type
               , level    :: Double -- ^ Security price level in that
                                    -- currency. (Zero in case of Rate).
               , currency :: String -- ^ Currency used in prices
               , security :: String -- ^ Security, like Bitcoin
               , exchange :: String -- ^ Exchange name
               } deriving (Show,Ord,Eq)

type Entry = (Key,Scientific)
