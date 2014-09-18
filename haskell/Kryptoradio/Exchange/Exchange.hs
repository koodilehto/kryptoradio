-- |Common functions between exchanges
module Kryptoradio.Exchange.Exchange where

data Kind = Bid | Trade | Ask | Rate deriving (Show,Ord,Eq)

data Key = Key { kind     :: Kind
               , price    :: Double -- Zero of kind is Rate. TODO change to scientific
               , currency :: String
               , exchange :: String
               } deriving (Show,Ord,Eq)

type Entry = (Key,Double) -- TODO change Double to Scientific
