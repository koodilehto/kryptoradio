-- |Common functions between exchanges
module Exchange where

data Kind = Bid | Ask | Trade deriving (Show)

data Key = Key { kind     :: Kind
               , price    :: Double -- TODO change to scientific
               , currency :: String
               , exchange :: String
               } deriving (Show)

type Entry = (Key,Double) -- TODO change Double to Scientific
