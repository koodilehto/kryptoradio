module Serialization where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

serializator reader = forever $ do
  x <- reader
  print x
  threadDelay ((fromIntegral $ B.length x) * 10^6)
  putStrLn "waiting more"
