{-# LANGUAGE ForeignFunctionInterface #-}
module Serial (openSerialRaw) where

import Control.Monad
import Foreign.C
import Foreign.C.Error
import System.IO
import System.Posix.IO
import System.Posix.Types
 
foreign import ccall "serial_open_raw" serial_open_raw :: CString -> CInt -> IO CInt
foreign import ccall "tcdrain" tcdrain :: CInt -> IO CInt

-- |Opens serial port using given speed and returns a Handle to
-- it. The catch is that this supports even the non-standard bit rates
-- like 250000 bps. Also, the device is opened in raw mode, meaning
-- that the input and output are not altered in operating system
-- level.
openSerialRaw :: FilePath -> Int -> IO (Handle,IO ())
openSerialRaw file speed = do
  fd_c <- withCString file $ \s -> throwErrnoIfMinus1Retry
                                   "Unable to open serial port" $
                                   serial_open_raw s (fromIntegral speed)
  h <- fdToHandle $ Fd fd_c
  return (h,drainSerial h fd_c)

-- |Flush and drain serial port buffers, including hardware
-- buffer. Given file descriptor and Handle must point to the same
-- underlying file!
drainSerial :: Handle -> CInt -> IO ()
drainSerial h fd = do
  hFlush h
  throwErrnoIfMinus1Retry_ "Unable to drain buffers" $ tcdrain fd
