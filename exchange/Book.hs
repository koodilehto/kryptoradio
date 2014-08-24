{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- |Order book and trade history tools
module Book where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad (forever)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Bitstamp
import Exchange

main = do
  ch <- bitstamp
  book <- newTVarIO M.empty
  forkIO $ orderbook ch book
  forever $ do
    getLine
    stuff <- readTVarIO book
    print stuff

orderbook :: TChan [Entry] -> TVar (Map Key Double) -> IO ()
orderbook ch book = forever $ atomically $ do
  xs <- readTChan ch
  orig <- readTVar book
  writeTVar book $ foldl (flip update) orig xs
  where update (key,value) = case (kind key,value) of
          (Trade,_) -> M.insertWith (+) key value
          (Bid,0)   -> M.delete key
          (Ask,0)   -> M.delete key
          (Bid,_)   -> M.insert key value
          (Ask,_)   -> M.insert key value

