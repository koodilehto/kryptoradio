{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Broadcaster where

import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Network.HTTP.Types (ok200,badRequest400)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Resources

main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port $ app resources
 
app :: [Resource] -> Application
app res req = case (requestMethod req,pathResource $ pathInfo req) of
  ("GET",Just name)     -> good $ describe name
  ("GET",Nothing)       -> good $ describeAll resources
  ("PUT",Just name)     -> good $ "TODO\n"
  ("REPLACE",Just name) -> good $ "TODO\n"
  ("DELETE",Just name)  -> good $ "TODO\n"
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

binaryResponse x = return $
                   responseLBS ok200
                   [("Content-Type", "application/octet-stream")]
                   x
