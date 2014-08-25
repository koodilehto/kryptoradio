-- |Common functions between exchanges
module Exchange where

data Kind = Bid | Trade | Ask deriving (Show,Ord,Eq)

data Key = Key { kind     :: Kind
               , price    :: Double -- TODO change to scientific
               , currency :: String
               , exchange :: String
               } deriving (Show,Ord,Eq)

type Entry = (Key,Double) -- TODO change Double to Scientific
