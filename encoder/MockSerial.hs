module MockSerial (openMockSerialOut) where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as B
import System.IO
import Data.Int (Int64)

-- |Opens "mock" serial port for output in binary mode. Emulate serial
-- port speed if speed is not zero. Although it is a mock device, the
-- data is really written to the given file.
openMockSerialOut :: FilePath -> Int -> IO (IO (), B.ByteString -> IO ())
openMockSerialOut file speed = do
  h <- openBinaryFile file WriteMode
  let write bs = do
        B.hPut h bs
        hFlush h
        unless (speed == 0) $ emulateLag speed $ B.length bs
    in return (hClose h, write)

-- |Emulate transmission speed using 10 bits per byte (start bit + 8
-- bits + 1 stop bit) and given baud rate.
emulateLag :: Int -> Int64 -> IO ()
emulateLag baud bytes =
  -- Making sure it doesn't break if we have 32 bit CPU.
  threadDelay $ fromIntegral $ 10000000 * bytes `div` toInt64 baud
  where toInt64 :: Int -> Int64
        toInt64 = fromIntegral
