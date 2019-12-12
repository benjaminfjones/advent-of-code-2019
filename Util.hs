module Util where

readInts :: String  -- ^ filepath
        -> IO [Int]
readInts fp =
    map readInt . lines <$> readFile fp
  where
    readInt :: String -> Int
    readInt = read
