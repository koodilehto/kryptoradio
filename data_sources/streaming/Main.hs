{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
-- |Encoder for any kind of streaming content with some kind of
-- separators.
module Main where

import qualified Data.ByteString.Char8 as BS
import System.Console.CmdArgs.Implicit
import Network.Curl
import Data.Word
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Control.Monad
import Data.List

data Args = Args { url       :: String
                 } deriving (Show, Data, Typeable)

synopsis =
  Args { url = def &= argPos 0 &= typ "URL"
       }
  &= program "kryptoradio-streamer"
  &= summary "Kryptoradio Streamer v0.0.1"
  &= help "Listens to incoming data in standard input and sends it to \
          \Kryptoradio Encoder"

main = do
  Args{..} <- cmdArgs synopsis
  unsent <- newTVarIO []
  hSetBuffering stdin LineBuffering
  
  forever $ do
    bs <- BS.getLine
    atomically $ modifyTVar unsent (bs:)
    forkIO $ update url unsent

update url var = do
  origList <- readTVarIO var
  let newStuff = BS.intercalate "," $ reverse origList
  (status,_) <- curlGetString url
                [CurlCustomRequest "PUT", CurlPostFields [BS.unpack newStuff]]
  case status of
    CurlOK -> do
      atomically $ do
        curList <- readTVar var
        if isSuffixOf origList curList
          then writeTVar var $ drop (length curList - length origList) curList
          else fail "Queued list has sporadically changed. Very bad."
      putStrLn "Updated"
    _ -> putStrLn "Discarded"
