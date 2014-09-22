-- | Tools for efficient serialization of Scientific.
module Kryptoradio.Exchange.ScifiTools where

import Data.Scientific
import Data.List
import Data.Function

-- |Serialization function which uses always the shortest form of
-- given scientific number. If two formats have equal length, then we
-- use the most readable format (i.e. normal decimal number) 
compactShow :: Scientific -> String
compactShow x = minimumBy (compare `on` length) (normal:compact:integer)
  where (ds,exp) = toDecimalDigits x
        tidyExp = exp - length ds
        fixSign (x:xs) = (x:map abs xs)
        normal = formatScientific Fixed Nothing x
        compact = concatMap show (fixSign ds) ++ "E" ++ show tidyExp
        integer = if fromInteger (floor x) == x then [show (floor x)] else [] 
