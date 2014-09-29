{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Kryptoradio.Receiver.Parser where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as A
import Data.Functor
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad (unless)
import System.IO
import Kryptoradio.Receiver.Resources

data Packet = Sync SyncPacket | Payload BL.ByteString
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
      unless (version==0) $ fail $ "Unknown Kryptoradio version: " ++ show version
      (s,Payload syncBs) <- krp $ ParseState (packetLeft-2) []
      case parseSyncPacket syncBs of
        Right a -> return (s, Sync a)
        Left a -> fail "Invalid format in sync packet"
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
krpToChan :: Handle -> TVar [Resource] -> IO ()
krpToChan h var =
  manyParse krp (ParseState 0 []) (B.hGetSome h 8192) (handlePacket var) B.empty

-- |Processes a single Kryptoradio packet, including Sync.
handlePacket :: TVar [Resource] -> Packet -> IO ()
handlePacket resVar (Sync new) = atomically $ do
  old <- readTVar resVar
  retrofitted <- retrofit old new
  writeTVar resVar retrofitted
handlePacket resVar (Payload bs) = atomically $ do
  res <- readTVar resVar
  let Just (packetRid,payload) = BL.uncons bs
      toPair Resource{..} = (rid,var)
      findVar = lookup packetRid $ map toPair res
  case findVar of
    Nothing -> return ()
    Just chan -> writeTChan chan payload

-- |Executes parser multiple times using the same reader function and
-- passes the output to given action. Last parameter is initial input,
-- in which empty bytestring is is fine.
manyParse :: (Monad m, Show s) => (s -> A.Parser (s,a)) -> s -> m B.ByteString -> (a -> m ()) -> B.ByteString -> m ()
manyParse parser state read write acc = do
  res <- A.parseWith read (parser state) acc
  case res of
    A.Done i (newState,r) -> do
      write r
      manyParse parser newState read write i
    A.Partial _ -> fail "Device disconnected?"
    A.Fail t s m -> fail $ unwords [ "Fail", show t, show s, show m] 
