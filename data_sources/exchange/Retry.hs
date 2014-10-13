-- |Helper functions for retrying, for example a connection.
module Retry where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (when)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- |This function runs given action infinitely until it succeeds. On
-- every failure, reporter function is called. If it fails before 5
-- minutes, keep a 5 minute break before retrying. Otherwise retry
-- immediately.
foreverRetry :: (Exception e) => IO a -> (e -> IO ()) -> IO a
foreverRetry act reporter = do
  before <- getPOSIXTime
  result <- try act
  case result of
    Right a -> return a -- Graceful exit
    Left e -> do
      after <- getPOSIXTime
      reporter e
      -- 5 minutes delay if last fail was less than 5 minutes ago
      when (after - before < 300) $ threadDelay 300000000
      foreverRetry act reporter

-- |Simple retry reporter if you don't want to write one
-- yourself. This also makes this receive all kinds of exceptions
-- (SomeException).
printException :: SomeException -> IO ()
printException e = putStrLn $ "Exception received (retrying): " ++ show e

-- |Shorthand for an easy retry.
foreverRetryPrintEx :: IO a -> IO a
foreverRetryPrintEx = flip foreverRetry printException
