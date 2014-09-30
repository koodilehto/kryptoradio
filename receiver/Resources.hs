{-# LANGUAGE RecordWildCards #-}
module Resources
       ( Resource(..)
       , SyncPacket
       , retrofit
       , parseSyncPacket
       ) where

import Control.Applicative
import Data.Word
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

type SyncPacket = [RawResource]

type RawResource = TChan ByteString -> Resource

data Resource = Resource { rid      :: Word8
                         , name     :: Text
                         , desc     :: Text
                         , var      :: TChan ByteString
                         }

-- |Uses previously generated channels for new resource set. If no
-- channel is available for that resource, then create a new. When
-- calling the first time, it is safe to pass empty list in "old".
retrofit :: [Resource] -> SyncPacket -> STM [Resource]
retrofit old new = mapM addVar new
  where
    findVar = flip lookup $ map pair old
    pair Resource{..} = (name,var)
    addVar raw = case findVar $ name $ raw undefined of
      Just x  -> return $ raw x
      Nothing -> raw <$> newBroadcastTChan

-- |Parses given packet 
parseSyncPacket :: ByteString -> Either String SyncPacket
parseSyncPacket = A.eitherResult . A.parse resources

-- |Parse whole resource set
resources :: A.Parser [RawResource]
resources = many resource

-- |Parse a single resource
resource :: A.Parser RawResource
resource = Resource <$> A.anyWord8 <*> cString <*> cString

-- |Parse C string (null-terminated text) with UTF-8 encoding
cString :: A.Parser Text
cString = do
  bs <- A.takeWhile (/= 0)
  A.word8 0
  return $ decodeUtf8 bs
