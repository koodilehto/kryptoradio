{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
module Resources where

import Blaze.ByteString.Builder
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Functor
import Data.Monoid
import Data.Text (Text,unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word
import GHC.Exts (sortWith)

data Delivery = Start    -- ^ Beginning state. Never stored in TVar.
              | Waiting  -- ^ The data is enqueued.
              | Replaced -- ^ Not delivered. Replaced or deleted.
              | Sending  -- ^ Parts of data is already sent.
              | Sent     -- ^ The data has been delivered.
              deriving (Eq,Show)

type Content = (TVar Delivery,BL.ByteString)

type RawResource = TMVar Content -> Resource

data Resource = Resource { rid       :: Word8
                         , priority  :: Word8
                         , zlibGroup :: Word8
                         , sign      :: Bool
                         , name      :: Text
                         , desc      :: Text
                         , var       :: TMVar Content
                         }

data Conf = Conf { resources :: [Resource]
                 }

rawResources :: [RawResource]
rawResources =
  [Resource 0 0 0 False "control" "Kryptoradio control channel"
  ,Resource 1 2 0 False "bitcoin" "Bitoin packet (transactions and blocks)"
  ,Resource 2 4 0 False "exchange" "Currency exchange data (Bitstamp and Bitpay) including order book"
  ,Resource 3 1 0 False "fimk" "FIMKrypto block explorer dump (transactions and blocks)"
  ,Resource 4 5 0 False "qsl" "QSL verification codes"
  ,Resource 5 6 0 False "irc" "Internet Relay Chat (subscribed channels only)"
  ]

-- |Create transactional variables from raw resource text.
newConf :: [RawResource] -> IO Conf
newConf raws = Conf <$> (mapM (<$> newEmptyTMVarIO) raws)

-- |Get resource id and new message using correct priority. 
priorityTake :: [Resource] -> STM (Resource,Content)
priorityTake res = foldr1 orElse $ map f $ sortWith priority res
  where f r = (r,) <$> takeTMVar (var r)

buildResource :: Resource -> Builder
buildResource Resource{..} = fromWord8 rid <>
                             fromWord8 zlibGroup <>
                             fromWord8 (if sign then 1 else 0) <>
                             cString name <>
                             cString desc <>
                             fromByteString "TODO: SHA256 HASH OF LAST PACKET" -- Dummy str of 256 bits

-- |Outputs Kryptoradio sync packet
syncPacket :: Conf -> IO BL.ByteString
syncPacket Conf{..} = do
  timestamp <- getTimestamp
  return $ toLazyByteString $
    binaryBlob "TODO PUBLIC KEY" <>
    binaryBlob "TODO SIGNATURE" <>
    fromByteString "TODO: SHA256 HASH OF LAST SYNC  " <> -- Dummy str of 256 bits
    timestamp <>
    foldr (mappend.buildResource) mempty resources

-- |C style string: encoded in UTF-8 and terminated by null byte (\0)
cString :: Text -> Builder
cString x = (fromByteString $ encodeUtf8 x) <> nul

nul :: Builder
nul = fromWord8 0

showText :: Text -> ShowS
showText = showString . unpack

-- |ByteString of given length. Maximum string length is 255 bytes.
binaryBlob :: BL.ByteString -> Builder
binaryBlob x = fromWord8 (fromIntegral $ BL.length x) <>
               fromLazyByteString x

-- |Current time with microsecond precision
getTimestamp :: IO Builder
getTimestamp = do
  ts <- getPOSIXTime 
  return $ fromWord64be $ floor $ ts * 1e6
