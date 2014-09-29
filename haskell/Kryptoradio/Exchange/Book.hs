{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
-- |Order book and trade history tools
module Main where

import Control.Exception
import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad (forever)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Scientific
import Network.Curl
import System.Console.CmdArgs.Implicit hiding (name)
import Kryptoradio.Exchange.Bitstamp
import Kryptoradio.Exchange.BitPay
import Kryptoradio.Exchange.Exchange
import Kryptoradio.Exchange.ScifiTools

-- |Do something if a thread dies.
bomb :: (a -> c) -> Either SomeException b -> c
bomb act = act . either throw (const $ error "Thread died")

defUrl = "http://localhost:3000/exchange"

data Args = Args { url    :: String
                 } deriving (Show, Data, Typeable)

synopsis = Args { url = defUrl &= help ("Kryptoradio Core URL (default: "++
                                        defUrl++")")
                }
           &= program "kryptoradio-exchange"
           &= summary "Kryptoradio Exchange Information v0.0.1"
           &= help "Connects to BitPay for exchange rates and Bitstamp for \
                   \order book. Sends data to Kryptoradio Core at given URL"

main = do
  Args{..} <- cmdArgs synopsis
  ch <- newTChanIO
  book <- newTVarIO M.empty
  sentBook <- newTVarIO M.empty
  -- safefork makes sure that the exception is listened on the main loop
  let safeFork a = forkFinally a $ bomb $ atomically.writeTVar book
  safeFork $ bitstamp ch
  safeFork $ bitpay ch
  safeFork $ orderbook ch book
  -- Loop for changes in order book and fork updater for it
  let loop old = do
        waitNew book old
        -- New data, trying to send it
        new <- readTVarIO book
        forkIO $ updateBook url sentBook new
        -- Not interested if it fails or not
        loop new
    in loop M.empty

-- Updates order book to Kryptoradio core and mark it as sent if it
-- was successful (= not replaced)
updateBook :: String -> TVar (Map Key Scientific) -> Map Key Scientific -> IO ()
updateBook url sentVar new = do
  -- Deciding what to send. Not perfectly threads safe but will work
  -- most of the time.
  sent <- readTVarIO sentVar
  let newStuff = unlines' $ map pairToCsv $ M.toList $ bookDiff sent new
  (status,_) <- curlGetString url
                [CurlCustomRequest "PUT",CurlPostFields [newStuff]]
  case status of
    CurlOK -> do
      atomically $ writeTVar sentVar new
      putStrLn "Updated"
    _ -> putStrLn "Discarded"

orderbook :: TChan [Entry] -> TVar (Map Key Scientific) -> IO ()
orderbook ch book = forever $ atomically $ do
  xs <- readTChan ch
  orig <- readTVar book
  writeTVar book $ foldl (flip update) orig xs
  where update (key,value) = case (kind key,value) of
          (Trade,_) -> M.insertWith (+) key value
          (Bid,0)   -> M.delete key
          (Ask,0)   -> M.delete key
          _         -> M.insert key value

bookDiff :: Map Key Scientific -> Map Key Scientific -> Map Key Scientific
bookDiff = M.mergeWithKey common onlyInOld onlyInNew
  where
    -- If a value is in both old and new
    common Key{..} a b = case (kind,a==b) of
      (_,True)  -> Nothing    -- No changes, not interesting
      (Trade,_) -> Just (b-a) -- In cases of trades, send change
      _         -> Just b     -- In cases of bid, ask, and rate: use newer value
    -- If a value is absent from new, it is a sign that bid/ask is
    -- removed. Then we give a value of zero. Trades are never removed
    -- so they are not checked.
    onlyInOld = M.map (const 0)
    -- Keep new trades and orders if present only in new
    onlyInNew = id

fullFlush :: Map Key a -> Map Key a
fullFlush = M.filterWithKey f
  where f Key{..} value = case kind of
          Trade -> True  -- Keep trades
          _     -> False -- Remove bids, asks, and rates

pairToCsv (Key{..},amount) =
  intercalate "," $ case (kind,amount) of
    (Rate,_) -> [exchange,security,currency,"R",compactShow amount]
    (_,0)    -> [exchange,security,currency,kindStr,show level]
    _        -> [exchange,security,currency,kindStr,show level,compactShow amount]
  where kindStr = case kind of
          Bid   -> "B"
          Ask   -> "A"
          Trade -> "T"

waitNew :: Eq a => TVar a -> a -> IO ()
waitNew var old = do
  atomically $ do
    new <- readTVar var
    check $ old /= new
  putStrLn "New data available"

-- |Like `unlines` but doesn't put newline after last element.
unlines' :: [String] -> String
unlines' = intercalate "\n"
