{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
module Kryptoradio.Core.Resources where

import Blaze.ByteString.Builder
import Data.ByteString.Lazy.Char8 (ByteString,pack)
import Data.Text (Text,unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid
import Data.Word
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.Functor
import GHC.Exts (sortWith)

data Delivery = Start    -- ^ Beginning state. Never stored in TVar.
              | Waiting  -- ^ The data is enqueued.
              | Replaced -- ^ Not delivered. Replaced or deleted.
              | Sending  -- ^ Parts of data is already sent.
              | Sent     -- ^ The data has been delivered.
              deriving (Eq,Show)

type Content = (TVar Delivery,ByteString)

type RawResource = TMVar Content -> Resource

data Resource = Resource { rid      :: Word8
                         , name     :: Text
                         , priority :: Word8
                         , desc     :: Text
                         , var      :: TMVar Content
                         }

resources :: [RawResource]
resources = [Resource 0 "control" 0 "Kryptoradio control channel"
            ,Resource 1 "bitcoin" 2 "Bitoin packet (transactions and blocks)"
            ,Resource 2 "exchange" 4 "Currency exchange data (Bitstamp and Bitpay) including order book"
            ,Resource 3 "fimk" 1 "FIMKrypto block explorer dump (transactions and blocks)"
            ,Resource 4 "qsl" 5 "QSL verification codes"
            ,Resource 5 "irc" 6 "Internet Relay Chat (subscribed channels only)"
            ]

-- |Create transactional variables from raw resource text.
newResources :: [RawResource] -> IO [Resource]
newResources = mapM (<$> newEmptyTMVarIO)

-- |Get resource id and new message using correct priority. 
priorityTake :: [Resource] -> STM (Word8,Content)
priorityTake res = foldr1 orElse $ map f $ sortWith priority res
  where f Resource{..} = (rid,) <$> takeTMVar var

describe :: Resource -> ByteString
describe Resource{..} = pack $
                        showString   "resource id:  " $ shows rid $
                        showString "\nname:         " $ showText name $
                        showString "\npriority:     " $ shows priority $
                        showString "\ndescription:  " $ showText desc $
                        "\n"

describeAll :: [Resource] -> ByteString
describeAll x = pack $
                showString "Kryptoradio broadcaster is up and running.\nSupported resources: " $
                showList (map name x) "\n"


buildResource :: Resource -> Builder
buildResource Resource{..} = fromWord8 rid <>
                             cString name <>
                             cString desc

-- |Outputs Kryptoradio sync packet
syncPacket :: [Resource] -> ByteString
syncPacket = toLazyByteString . foldr (mappend.buildResource) mempty

-- |C style string: encoded in UTF-8 and terminated by null byte (\0)
cString :: Text -> Builder
cString x = (fromByteString $ encodeUtf8 x) <> nul

nul :: Builder
nul = fromWord8 0

showText :: Text -> ShowS
showText = showString . unpack
