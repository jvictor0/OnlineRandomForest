module Utils where

import Data.List
import Data.Random

isPowerOfTwo :: Int -> Bool
isPowerOfTwo 1 = True
isPowerOfTwo 0 = False
isPowerOfTwo n = n`mod`2 == 0 && (isPowerOfTwo $ n`div`2 )

sum' x = foldl' (+) 0 x

randomChoice g lst = lst!!(fst $ sampleState (uniform 0 (length lst - 1)) g)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing
  
x//y = (fromIntegral x)/(fromIntegral y)