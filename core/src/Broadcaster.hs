{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Monad (unless)
import Control.Monad.STM
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Network.HTTP.Types (ok200,badRequest400)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Resources
import Serialization
import SyncTimer

main = do
  let port = 3000
  res <- newResources resources
  timer <- newSyncTimer
  forkIO $ serializator timer $ priorityTake res
  putStrLn $ "Listening on port " ++ show port
  run port $ app res timer

app :: [Resource] -> SyncAct -> Application
app res timer req respond = case (requestMethod req,pathResource $ pathInfo req,pathInfo req) of
  ("GET",Just name,_) -> out ok200 $ describe name
  ("GET",_,[])        -> out ok200 $ describeAll res
  ("GET",_,[sync])    -> do
    i <- atomically timer
    atomically $ waitSync timer i
    out ok200 "SYNC\n"
  ("PUT",Just r,_) -> do
    -- Put message in a queue
    packet <- lazyRequestBody req
    ok <- atomically $ tryPutTMVar (var r) packet
    act <- atomically $ do
      e <- tryReadTMVar $ var r
      case (ok,e,Just packet==e) of
        (False,_,_)   -> return $ out badRequest400 "FULL\n"
        (_,Nothing,_) -> return $ out ok200 "SENDING\n"
        (_,_,False)   -> return $ out badRequest400 "REPLACED\n"
        (_,_,True)    -> retry
    act
  ("REPLACE",Just r,_) -> do
    -- Replace existing message with a new
    packet <- lazyRequestBody req
    ok <- atomically $ do
      e <- isEmptyTMVar (var r)
      unless e $ swapTMVar (var r) packet >> return ()
      return e
    act <- atomically $ do
      e <- tryReadTMVar $ var r
      case (ok,e,Just packet==e) of
        (True,_,_)    -> return $ out badRequest400 "EMPTY\n"
        (_,Nothing,_) -> return $ out ok200 "SENDING\n"
        (_,_,False)   -> return $ out badRequest400 "REPLACED\n"
        (_,_,True)    -> retry
    act
  ("DELETE",Just r,_) -> do
    -- Delete already queued message. NB! This incorrectly reports SENDING on the waiting request
    ok <- atomically $ tryTakeTMVar (var r)
    case ok of
      (Just _) -> out ok200 "DELETED\n"
      Nothing  -> out badRequest400 "EMPTY\n"
  _ -> out badRequest400 "Invalid request\n"
  where findResource = flip lookup $ map (\x -> (name x,x)) res
        pathResource [name] = findResource name
        pathResource _ = Nothing
        out code text = respond $
                        responseLBS code
                        [("Content-Type", "text/plain")] $
                        text
