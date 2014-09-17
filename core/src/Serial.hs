{-# LANGUAGE ForeignFunctionInterface #-}
module Serial (openSerialOutRaw) where

import Foreign.C
import System.IO
import System.Posix.IO
import System.Posix.Types
import Data.ByteString.Lazy (ByteString, hPut)

foreign import ccall "init_serial_port" init_serial_port :: CInt -> CInt -> IO CInt
foreign import ccall "tcdrain" tcdrain :: CInt -> IO CInt

-- |Opens serial port for output in raw 8-bit mode using given speed
-- and returns functions for closing and writing to the port. It
-- supports even the non-standard bit rates like 250000 bps. In raw
-- 8-bit mode the output is not altered by the operating system.
openSerialOutRaw :: FilePath -> Int -> IO (IO (), ByteString -> IO ())
openSerialOutRaw file speed = do
  Fd fd <- openFd file WriteOnly Nothing OpenFileFlags{ append    = False
                                                      , exclusive = False
                                                      , noctty    = True
                                                      , nonBlock  = False
                                                      , trunc     = False
                                                      }
  throwErrnoIfMinus1Retry_ "Unable to configure serial port" $
    init_serial_port fd (fromIntegral speed)

  h <- fdToHandle $ Fd fd
  return (hClose h, \bs -> hPut h bs >> hFlush h >> drainSerial fd)

-- |Drain serial port buffers, including hardware buffer. This
-- supports retrying if interrupted, unlike `drainOutput` in
-- System.Posix.Terminal.
drainSerial :: CInt -> IO ()
drainSerial fd = throwErrnoIfMinus1Retry_ "Unable to drain buffers" $ tcdrain fd
