{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Market.BitPay where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TChan
import Control.Monad (forever,mzero)
import Control.Monad.STM
import Data.Aeson
import Data.Scientific (Scientific)
import Network.Curl.Aeson

import Exchange

data BpRate = BpRate { code :: String
                     , name :: String
                     , rate :: Scientific
                     } deriving (Show)

instance FromJSON BpRate where
     parseJSON (Object v) = BpRate <$>
                            v .: "code" <*>
                            v .: "name" <*>
                            v .: "rate"
     parseJSON _          = mzero

getRates :: IO [BpRate]
getRates = curlAesonGet "https://bitpay.com/api/rates"

rateToEntry :: BpRate -> Entry
rateToEntry BpRate{..} = (Key Rate 0 code "XBT" "BITPAY",rate)

bitpay :: TChan [Entry] -> IO ()
bitpay chan = forever $ do
  xs <- getRates
  atomically $ writeTChan chan $ map rateToEntry xs
  -- Not a very good timing, but just wait for 30 seconds
  threadDelay 30000000
