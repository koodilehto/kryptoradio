{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Broadcaster where

import Control.Monad (unless)
import Control.Monad.STM
import Control.Monad.IO.Class
import Control.Concurrent.STM.TMVar
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Conduit
import Data.Conduit.Binary
import Network.HTTP.Types (ok200,badRequest400)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Resources

main = do
  let port = 3000
  res <- newResources resources
  putStrLn $ "Listening on port " ++ show port
  run port $ app res
 
app :: [Resource] -> Application
app res req = case (requestMethod req,pathResource $ pathInfo req) of
  ("GET",Just name)     -> good $ describe name
  ("GET",Nothing)       -> good $ describeAll res
  ("PUT",Just r) -> do
    -- Put message in a queue
    packet <- requestBody req $$ sinkLbs
    ok <- liftIO $ atomically $ tryPutTMVar (var r) packet
    liftIO $ atomically $ do
      e <- tryReadTMVar $ var r
      case (ok,e,Just packet==e) of
        (False,_,_)   -> bad "FULL\n"
        (_,Nothing,_) -> good "SENDING\n"
        (_,_,False)   -> bad "REPLACED\n"
        (_,_,True)    -> retry
  ("REPLACE",Just r) -> do
    -- Replace existing message with a new
    packet <- requestBody req $$ sinkLbs
    ok <- liftIO $ atomically $ do
      e <- isEmptyTMVar (var r)
      unless e $ swapTMVar (var r) packet >> return ()
      return e
    liftIO $ atomically $ do
      e <- tryReadTMVar $ var r
      case (ok,e,Just packet==e) of
        (True,_,_)    -> bad "EMPTY\n"
        (_,Nothing,_) -> good "SENDING\n"
        (_,_,False)   -> bad "REPLACED\n"
        (_,_,True)    -> retry
  ("DELETE",Just r) -> do
    -- Delete already queued message. NB! This incorrectly reports SENDING on the waiting request
    ok <- liftIO $ atomically $ tryTakeTMVar (var r)
    case ok of
      (Just _) -> good "DELETED\n"
      Nothing  -> bad "EMPTY\n"
  _ -> bad "Invalid request\n"
  where findResource = flip lookup $ map (\x -> (name x,x)) res
        pathResource [name] = findResource name
        pathResource _ = Nothing

bad,good :: Monad m => ByteString -> m Response
bad = textualResponse badRequest400
good = textualResponse ok200
 
textualResponse code text = return $
                            responseLBS code
                            [("Content-Type", "text/plain")] $
                            text
