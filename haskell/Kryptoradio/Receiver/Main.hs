module Main where

import Kryptoradio.Receiver.Dvb
import qualified Data.ByteString.Char8 as B

main = do
  putStrLn "It compiles!"
  (h,dvb) <- openDvb 0 0 0 634000000 8101
  putStrLn "eka:"
  bs <- B.hGet h 184
  print bs
  putStrLn "toka:"
  bs <- B.hGet h 184
  print bs
  closeDvb dvb
