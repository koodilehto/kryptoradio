{-# LANGUAGE OverloadedStrings #-}
module Kryptoradio.Receiver.Parser where

import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as A
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.IO

-- |Parser for PES packet
pes :: A.Parser B.ByteString
pes = do
  A.string "\NUL\NUL\SOH" -- Packet start code prefix
  A.anyWord8 -- Stream id (ignored)
  len <- A.anyWord16be
  A.take $ fromIntegral len

-- |Parses PES packets and put them in a channel.
pesToChan :: Handle -> TChan B.ByteString -> IO ()
pesToChan h chan =
  manyParse pes (B.hGetSome h 8192) (atomically . writeTChan chan) B.empty

-- |Executes parser multiple times using the same reader function and
-- passes the output to given action. Last parameter is initial input,
-- in which empty bytestring is is fine.
manyParse :: (Monad m, Show a) => A.Parser a -> m B.ByteString -> (a -> m ()) -> B.ByteString -> m ()
manyParse parser read write acc = do
  res <- A.parseWith read parser acc
  case res of
    A.Done i r -> do
      write r
      manyParse parser read write i
    _ -> fail $ "Parsing failed: " ++ show res
