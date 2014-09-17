module Serialization where

import Control.Monad (forever)
import Control.Monad.STM
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Functor
import Data.Word
import Data.Int
import System.IO (Handle,hFlush)
import SyncTimer
import Resources (Content,Delivery(..))

data ReadResult = Empty | Sync Integer | Packet (Word8,Content)

serializator :: SyncAct -> STM (Word8,Content) -> (ByteString -> IO ()) -> IO ()
serializator timer reader writeSerial = serializator' 0 0
  where
    serializator' pad i = do
      -- Get new data. If we have a fragment in buffer, do not wait for
      -- more data. Otherwise we block and wait.
      incoming <- atomically $ (Sync <$> waitSync timer i) `orElse` (Packet <$> reader) `orElse` waitIfEmpty

      let (report,bs) = case incoming of
            Empty -> (skip,trail) -- Pad everything
            Sync _ -> (skip,trail `B.append` syncPacket)
            Packet (rid,(delivery,msg)) ->
              (atomically.writeTVar delivery,toKRFs pad $ rid `B.cons` msg)

      -- Send it
      report Sending
      writeSerial bs
      report Sent

      -- Calculate new offset in PES packet
      let offset = (pad + B.length bs) `mod` klpSize
      putStrLn $ "waiting more, offset " ++ show offset
      serializator' offset $ case incoming of
        Sync next -> next
        _         -> i
      where
        trail       = if pad == 0 then B.empty else B.replicate (klpSize-pad) 0xfe
        waitIfEmpty = if pad == 0 then retry   else return Empty
        skip _ = return ()

-- |Split a Kryptoradio Resource Packet (KRP) to Kryptoradio Resource
-- Fragments (KRF) and concatenate everything.
toKRFs :: Int64 -> ByteString -> ByteString
toKRFs pos bs = if B.null next
                then fromIntegral (B.length cur) `B.cons` cur `B.append` pad
                else 0xff `B.cons` cur `B.append` toKRFs 0 next
  where (cur,next) = B.splitAt (fromIntegral $ klpSize-1-pos) bs
        pad = if pos + B.length cur == klpSize-2
              then B.singleton 0xfe
              else B.empty

-- |Kryptoradio link packet size is fixed to PES packet payload size
klpSize :: Int64
klpSize = 178

-- |Sync packet contains sync indicator and protocol version
syncPacket :: ByteString
syncPacket = B.pack [0x00,0x00]
