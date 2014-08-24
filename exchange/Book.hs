{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- |Order book and trade history tools
module Book where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad (forever)
import Data.List (intercalate)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Bitstamp
import Exchange

main = do
  ch <- bitstamp
  book <- newTVarIO M.empty
  forkIO $ orderbook ch book
  loop book M.empty
  where loop book old = do
          getLine
          new <- readTVarIO book
          putStrLn $ unlines $ map pairToCsv $ M.toList $ bookDiff old new
          loop book new

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

bookDiff :: Map Key Double -> Map Key Double -> Map Key Double
bookDiff = M.mergeWithKey common onlyInOld onlyInNew
  where
    -- If a value is in both old and new
    common Key{..} a b = case (kind,a==b) of
      (_,True)  -> Nothing    -- No changes, not interesting
      (Trade,_) -> Just (b-a) -- In cases of trades, send change
      (_,_)     -> Just b     -- In cases of bid and ask, use newer value
    -- If a value is absent from new, it is a sign that bid/ask is
    -- removed. Then we give a value of zero. Trades are never removed
    -- so they are not checked.
    onlyInOld = M.map (const 0)
    -- Keep new trades and orders if present only in new
    onlyInNew = id

pairToCsv (Key{..},amount) =
  intercalate "," [show kind,show price,show amount,currency,exchange]
