module AoCUtils.AoCParsing where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))

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

-- Extract integers (including negative) from mixed text
extractInts :: String -> [Int]
extractInts = mapMaybe readMaybe . getAllTextMatches . (=~ "-?[0-9]+")

-- Extract all numbers (integers and decimals, including negative) from mixed text  
extractNumbers :: String -> [Double]
extractNumbers = mapMaybe readMaybe . getAllTextMatches . (=~ "-?[0-9]+\\.?[0-9]*")