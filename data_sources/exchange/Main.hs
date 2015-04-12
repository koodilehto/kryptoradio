{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
-- |Order book and trade history tools
module Main where

import Control.Exception
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad (forever, unless, void, when)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Scientific
import Network
import System.Console.CmdArgs.Implicit
import System.Directory
import System.IO

import Market.Bitstamp
import Market.BitPay hiding (name)
import Exchange
import ScifiTools

-- |Do something if a thread dies.
bomb :: (a -> c) -> Either SomeException b -> c
bomb act = act . either throw (const $ error "Thread died")

data Args = Args { socket          :: String
                 , bitstamp_book   :: Bool
                 , bitstamp_trades :: Bool
                 , bitpay_rates    :: Bool
                 } deriving (Show, Data, Typeable)

synopsis defEncoder =
  Args { socket = def &= typFile &= name "u" &=
                  help ("Kryptoradio Encoder Unix domain socket (default: " ++
                        defEncoder ++ ")")
       , bitstamp_book = def &= name "S" &= help "Enable Bitstamp order book"
       , bitstamp_trades = def &= name "s" &= help "Enable Bitstamp trades"
       , bitpay_rates = def &= name "p" &= help "Enable BitPay rates"
       }
  &= program "kryptoradio-exchange"
  &= summary "Kryptoradio Exchange Information v0.0.1"
  &= help "Connects to BitPay for exchange rates and Bitstamp for \
          \order book. Sends data to Kryptoradio Encoder at given URL"

main = do
  defEncoder <- getDefSocket "kryptoradio-encoder"
  Args{..} <- cmdArgs $ synopsis defEncoder
  -- Some sanity checks
  unless (bitstamp_book || bitstamp_trades || bitpay_rates) $
    fail "You need to define at least one data source. To get them all, use -Ssp"
  -- Prepare transactional variables
  ch <- newTChanIO
  book <- newTVarIO M.empty
  sentBook <- newTVarIO M.empty
  -- Open connection to Kryptoradio encoder
  out <- connectTo undefined $ UnixSocket $ if null socket then defEncoder else socket
  -- `safefork` makes sure that the exception is listened on the main
  -- loop.
  let safeFork a = void $ forkFinally a $ bomb $ atomically.writeTVar book
  -- Fork sources that user wants
  when (bitstamp_book || bitstamp_trades) $
    safeFork $ bitstamp bitstamp_book bitstamp_trades ch
  when bitpay_rates $ safeFork $ bitpay ch
  safeFork $ orderbook ch book
  -- Loop for changes in order book and fork updater for it
  let loop lastBook sentBook = do
        -- Wait for changes and then fetch the new one
        newBook <- waitNew book lastBook
        -- Check last delivery status
        deleted <- isDeleted out
        putStrLn $ if deleted then "Discarded" else "Updated"
        let refBook = if deleted then sentBook else lastBook
        -- Construct new content. FIXME: Now assuming there is only
        -- ASCII (this uses ordinary Strings instead of ByteStrings)
        let packet = unlines' $ map pairToCsv $ M.toList $ bookDiff refBook newBook
        hPrint out $ length packet
        hPutStr out packet
        -- Call again using the newest order book and last delivered one
        loop newBook refBook
    in loop M.empty M.empty

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

-- |Wait for new value in TVar which is different than the one already
-- there. Returns new value.
waitNew :: Eq a => TVar a -> a -> IO a
waitNew var old = atomically $ do
  new <- readTVar var
  check $ old /= new
  return new

-- |Like `unlines` but doesn't put newline after last element.
unlines' :: [String] -> String
unlines' = intercalate "\n"

getDefSocket app = do
  path <- getAppUserDataDirectory app
  return $ path ++ "/" ++ "exchange"

-- |Returns True if previously sent element was dequeued or False
-- otherwise. Blocks until it gets a response from main thread.
isDeleted :: Handle -> IO Bool
isDeleted h = do
  hPutChar h 'D'
  loop
  where loop = do
          c <- hGetChar h
          case c of
            'R' -> return False
            'D' -> return True
            _ -> loop
