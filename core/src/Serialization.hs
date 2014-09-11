module Serialization where

import Control.Monad (forever)
import Control.Monad.STM
import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Functor
import Data.Word
import Data.Int
import SyncTimer

serializator :: SyncVar -> STM (Word8,ByteString) -> IO ()
serializator timer reader = serializator' 0
  where
    serializator' pad = do
      -- Get new data. If we have a fragment in buffer, do not wait for
      -- more data. Otherwise we block and wait.
      mbData <- atomically $ (Just <$> reader) `orElse` if pad == 0
                                                        then retry
                                                        else return Nothing
      -- Prepare data
      let trail = if pad==0 then B.empty else B.replicate (klpSize-pad) 0xfe
      bs <- case mbData of
        Nothing -> return trail
        Just (rid,x) -> do
          let krp = rid `B.cons` x -- TODO zlib, ecdsa

          -- Let's do syncing if needed
          sync <- maybeReset timer

          -- If synced, pad previous packet and put version header
          -- first. Otherwise just throw data in without padding.
          return $ if sync
                   then B.concat [trail,syncPacket,toKRFs 2 krp]
                   else toKRFs pad krp
      -- "Send" it
      print bs
      threadDelay ((fromIntegral $ B.length bs) * 10^3)
      -- TODO tcdrain()
      -- now we must check if get something to pad with
      let offset = (pad + B.length bs) `mod` klpSize
      putStrLn $ "waiting more, offset " ++ show offset
      serializator' offset

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
