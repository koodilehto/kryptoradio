{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
-- |Encoder for any kind of streaming content with some kind of
-- separators.
module Main where

import qualified Data.ByteString.Char8 as BS
import Network
import System.Console.CmdArgs.Implicit
import System.IO

data Args = Args { socket :: String
                 } deriving (Show, Data, Typeable)

synopsis =
  Args { socket = def &= argPos 0 &= typ "SOCKET"
       }
  &= program "kryptoradio-streamer"
  &= summary "Kryptoradio Streamer v0.0.1"
  &= help "Listens to incoming data in standard input and sends it to \
          \Kryptoradio Encoder"

main = do
  Args{..} <- cmdArgs synopsis
  out <- connectTo undefined $ UnixSocket socket
  hSetBuffering stdin LineBuffering

  let loop queue = do
        bs <- BS.getLine
        left <- update out (bs:queue)
        loop left
    in loop []

-- |Updates data in encoder and returns remaining buffer.
update :: Handle -> [BS.ByteString] -> IO [BS.ByteString]
update h queue = do
  -- Try to delete last and get report
  deleted <- isDeleted h
  let sendList = case deleted of
        True -> queue
        False -> [head queue]
      newStuff = BS.intercalate "\n" $ reverse sendList
  
  -- Send to socket
  hPrint h $ BS.length newStuff
  BS.hPut h newStuff

  -- Return remaining buffer
  return sendList

-- |Returns True if previously sent element was dequeued or False
-- otherwise. Blocks until it gets a response from main thread.
isDeleted :: Handle -> IO Bool
isDeleted h = do
  BS.hPut h "D"
  loop
  where loop = do
          c <- BS.hGet h 1
          case c of
            "R" -> return False
            "D" -> return True
            _ -> loop
