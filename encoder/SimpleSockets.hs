-- |Simple Socket IO for UNIX domain sockets without massive
-- dependencies like conduits.
module SimpleSockets (foreverAccept) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception.Base (SomeException, catch, finally, bracket)
import Control.Monad (forever)
import Network.Socket
import System.IO
import System.Posix (removeLink)

-- |Accepts incoming connections, only one at a time. Forks thread for
-- the handler.
foreverAccept :: (Handle -> IO ()) -> String -> IO ThreadId
foreverAccept act unix = do
  s <- open
  forkIO $ finally (loop s) (cleanup s)
  where
    open = do
      sock <- socket AF_UNIX Stream defaultProtocol
      bindSocket sock $ SockAddrUnix unix
      -- One pending connection is enough because it help us to detect
      -- misbehaviour in data sources.
      listen sock 1
      return sock
    loop sock = forever $ accept sock >>= withHandle (safe . act)
    cleanup s = do
      close s
      removeLink unix

-- |Makes a handle out of a socket for easier IO. This takes care of
-- closing the handle after action is finished.
withHandle :: (Handle -> IO ()) -> (Socket, SockAddr) -> IO ()
withHandle act (s,_) = bracket (socketToHandle s ReadWriteMode) hClose act

-- |Run IO action and just log the exception and continue as nothing happened
safe :: IO () -> IO ()
safe act = catch act eh
  where
    eh :: SomeException -> IO ()
    eh e = putStrLn $ "Handler died: " ++ show e
