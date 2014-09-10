module Serialization where

import Control.Monad (forever)
import Control.Monad.STM
import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Functor
import Data.Word
import Data.Int

serializator :: Int64 -> STM ByteString -> IO ()
serializator pad reader = do
  -- Get new data. If we have a fragment in buffer, do not wait for
  -- more data. Otherwise we block and wait.
  mbData <- atomically $ (Just <$> reader) `orElse` if pad == 0
                                                    then retry
                                                    else return Nothing
  -- Prepare data
  let bs = case mbData of
        Nothing -> B.replicate (178-pad) 0xfe
        Just x -> toKRFs pad x -- TODO zlib, ecdsa
  -- "Send" it
  print bs
  threadDelay ((fromIntegral $ B.length bs) * 10^3)
  -- TODO tcdrain()
  -- now we must check if get something to pad with
  let offset = (pad + B.length bs) `mod` 178
  putStrLn $ "waiting more, offset " ++ show offset
  serializator offset reader

-- |Split a Kryptoradio Resource Packet (KRP) to Kryptoradio Resource
-- Fragments (KRF) and concatenate everything.
toKRFs :: Int64 -> ByteString -> ByteString
toKRFs pos bs = if B.null next
                then fromIntegral (B.length cur) `B.cons` bs `B.append` pad
                else 0xff `B.cons` cur `B.append` toKRFs 0 next
  where (cur,next) = B.splitAt (fromIntegral $ 177-pos) bs
        pad = if pos + B.length cur == 177
              then B.singleton 0xfe
              else B.empty
