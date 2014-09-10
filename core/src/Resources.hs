{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}
module Resources where

import Data.ByteString.Lazy.Char8 (ByteString,pack)
import Data.Text (Text,unpack)
import Data.Word
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Data.Functor
import GHC.Exts (sortWith)

type RawResource = TMVar ByteString -> Resource

data Resource = Resource { rid      :: Word8
                         , name     :: Text
                         , priority :: Word8
                         , desc     :: Text
                         , var      :: TMVar ByteString
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
priorityTake :: [Resource] -> STM (Word8,ByteString)
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

showText :: Text -> ShowS
showText = showString . unpack
