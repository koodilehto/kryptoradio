{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Resources where

import Data.ByteString.Lazy.Char8 (ByteString,pack)
import Data.Text (Text,unpack)
import Data.Word

data Resource = Resource { rid      :: Word8
                         , name     :: Text
                         , priority :: Word8
                         , desc     :: Text
                         } deriving (Show)

resources = [Resource 0 "control" 0 "Kryptoradio control channel"
            ,Resource 1 "bitcoin" 2 "Bitoin packet (transactions and blocks)"
            ,Resource 2 "exchange" 4 "Currency exchange data (Bitstamp and Bitpay) including order book"
            ,Resource 3 "fimk" 1 "FIMKrypto block explorer dump (transactions and blocks)"
            ,Resource 4 "qsl" 5 "QSL verification codes"
            ,Resource 5 "irc" 6 "Internet Relay Chat (subscribed channels only)"
            ]

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
