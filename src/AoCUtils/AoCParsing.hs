module AoCUtils.AoCParsing where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- Common parsing utilities
parseNumbers :: String -> [Int]
parseNumbers = mapMaybe readMaybe . words

parseInt :: String -> Maybe Int
parseInt = readMaybe

-- Generic splitOn that works for any list type
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn sep (x:xs)
  | x == sep = [] : splitOn sep xs
  | otherwise = case splitOn sep xs of
      (y:ys) -> (x:y):ys
      [] -> [[x]]

-- Split list at first occurrence of separator, returning (before, after)
-- The separator is not included in either part
splitAtSep :: Eq a => a -> [a] -> ([a], [a])
splitAtSep sep xs = 
  let (before, after) = break (== sep) xs
  in (before, drop 1 after)