{-# LANGUAGE OverloadedStrings #-}
module Kryptoradio.Receiver.Parser where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as A
import Data.Functor
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.IO

data Packet = Sync | Payload BL.ByteString deriving (Show)
data ParseState = ParseState Int [B.ByteString] deriving (Show)

-- |Parser for PES packet header. Returns payload length.
pesHeader :: A.Parser Int
pesHeader = do
  A.string "\NUL\NUL\SOH" -- Packet start code prefix
  A.anyWord8 -- Stream id (ignored)
  fromIntegral <$> A.anyWord16be

-- |Parser for Kryptoradio packet
krp :: ParseState -> A.Parser (ParseState, Packet)
krp (ParseState lastPacketLeft fragments) = do
  -- Read header if we have fully consumed last packet
  packetLeft <- if lastPacketLeft == 0
                then pesHeader
                else return lastPacketLeft

  -- Read fragment header byte 
  fragmentInfo <- fromIntegral <$> A.anyWord8
  case fragmentInfo of
    0 -> do
      -- Sync packet
      version <- A.anyWord8
      -- TODO: Do something with the version
      return (ParseState (packetLeft-2) [],Sync)
    254 -> do
      -- Discard the rest
      A.take $ packetLeft-1
      krp $ ParseState 0 []
    255 -> do
      -- Take all data and continue to next PES packet
      bs <- A.take $ packetLeft-1
      krp $ ParseState 0 $ bs:fragments
    _ -> do
      -- Take one fragment and keep the rest
      bs <- A.take fragmentInfo
      return (ParseState (packetLeft-fragmentInfo-1) [],
              toPacket $ bs:fragments)

-- |Pack Kryptoradio fragments in a single ByteString. This just
-- concatenates bytestrings in reverse order because the list is
-- constructed in reverse order in `krp`.
toPacket :: [B.ByteString] -> Packet
toPacket = Payload . BL.fromChunks . reverse

-- |Parses PES packets and put them in a channel.
krpToChan :: Handle -> TChan Packet -> IO ()
krpToChan h chan =
  manyParse krp (ParseState 0 []) (B.hGetSome h 8192) (atomically . writeTChan chan) B.empty

-- |Executes parser multiple times using the same reader function and
-- passes the output to given action. Last parameter is initial input,
-- in which empty bytestring is is fine.
manyParse :: (Monad m, Show a, Show s) => (s -> A.Parser (s,a)) -> s -> m B.ByteString -> (a -> m ()) -> B.ByteString -> m ()
manyParse parser state read write acc = do
  res <- A.parseWith read (parser state) acc
  case res of
    A.Done i (newState,r) -> do
      write r
      manyParse parser newState read write i
    _ -> fail $ "Parsing failed: " ++ show res
