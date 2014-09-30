-- |Wrappers for libdvb. I'm not sure how it should be done so this
-- may contain fatal errors. Feel free to fix my code if it's broken.
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Dvb where

import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.IO
import System.Posix.IO (fdToHandle)
import System.Posix.Types

data DvbDeviceStruct
type DvbDevice = Ptr DvbDeviceStruct
  
foreign import ccall unsafe "new_dvb_device" new_dvb_device :: IO DvbDevice
foreign import ccall unsafe "dvb_open" dvb_open :: DvbDevice -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "dvb_get_error" dvb_get_error :: DvbDevice -> IO CString
foreign import ccall unsafe "dvb_tune" dvb_tune :: DvbDevice -> CSize -> IO CInt
foreign import ccall unsafe "dvb_init_pes_stream" dvb_init_pes_stream :: DvbDevice -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "dvb_stop_stream" dvb_stop_stream :: DvbDevice -> IO CInt
foreign import ccall unsafe "dvb_get_demuxer_fd" dvb_get_demuxer_fd :: DvbDevice -> IO CInt

openDvb :: Int -> Int -> Int -> Int -> Int -> IO (Handle,DvbDevice)
openDvb devId frontendId demuxerId frequency pid = do
  dev <- new_dvb_device
  when (dev==nullPtr) $ fail "Unable to allocate memory"
  openStatus <- dvb_open dev (fromIntegral devId) (fromIntegral frontendId) (fromIntegral demuxerId)
  case openStatus of
    0 -> do
      tuneStatus <- dvb_tune dev (fromIntegral frequency)
      case tuneStatus of
        0 -> do
          initStatus <- dvb_init_pes_stream dev (fromIntegral pid) 5
          case initStatus of
            0 -> do
              fd <- dvb_get_demuxer_fd dev
              h <- fdToHandle $ Fd fd
              return (h,dev)
            _ -> cleanupAndFail dev
        _ -> cleanupAndFail dev
    _ -> cleanupAndFail dev

cleanupAndFail :: DvbDevice -> IO a
cleanupAndFail dev = do
  msg <- dvb_get_error dev >>= peekCString
  free dev
  error msg

closeDvb :: DvbDevice -> IO ()
closeDvb dev = do
  status <- dvb_stop_stream dev
  case status of
    0 -> free dev
    _ -> cleanupAndFail dev
