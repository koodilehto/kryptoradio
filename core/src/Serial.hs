{-# LANGUAGE ForeignFunctionInterface #-}
module Serial (openSerialRaw) where

import Control.Monad
import Foreign.C
import System.IO
import System.Posix.IO
import System.Posix.Types
 
foreign import ccall "serial_open_raw" serial_open_raw :: CString -> CInt -> IO CInt

-- |Opens serial port using given speed and returns a Handle to
-- it. The catch is that this supports even the non-standard bit rates
-- like 250000 bps. Also, the device is opened in raw mode, meaning
-- that the input and output are not altered in operating system
-- level.
openSerialRaw :: FilePath -> Int -> IO Handle
openSerialRaw file speed = do
  fd_c <- withCString file $ flip serial_open_raw (fromIntegral speed)
  when (fd_c == -1) $ throwErrno "Unable to open serial port"
  fdToHandle $ Fd fd_c
