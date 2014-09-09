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
app res req = case (requestMethod req,pathInfo req) of
  ("GET",[])         -> good $ describeAll resources
  ("GET",[name])     -> good $ describe $ findResource name
  ("PUT",[name])     -> good $ "TODO " ++ show name ++ "\n"
  ("REPLACE",[name]) -> good $ "TODO " ++ show name ++ "\n"
  ("DELETE",[name])  -> good $ "TODO " ++ show name ++ "\n"
  _ -> unknown
  where unknown = bad "Unknown command\n"
        findResource = flip lookup $ map (\x -> (name x,x)) res

bad,good :: Monad m => String -> m Response
bad = textualResponse badRequest400
good = textualResponse ok200
 
textualResponse code text = return $
                            responseLBS code
                            [("Content-Type", "text/plain")] $
                            B.pack text

binaryResponse x = return $
                   responseLBS ok200
                   [("Content-Type", "application/octet-stream")]
                   x
