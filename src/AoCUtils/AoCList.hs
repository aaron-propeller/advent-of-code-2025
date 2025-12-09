module AoCUtils.AoCList (
    count, safeHead, safeTail,
    chunks, windows, freqMap,
    iterateUntilStable, uniquePairs
) where

import qualified Data.Map as Map

-- Common utility functions
count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- List utilities
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

windows :: Int -> [a] -> [[a]]
windows n xs = map (take n) (take (length xs - n + 1) (iterate tail xs))


-- Create frequency map from list
freqMap :: Ord a => [a] -> Map.Map a Int
freqMap = Map.fromListWith (+) . map (\x -> (x, 1))

-- Iterate function until result stabilizes (no change between iterations)
iterateUntilStable :: Eq a => (a -> a) -> a -> a
iterateUntilStable f initial = 
  let states = iterate f initial
      pairs = zip states (tail states)
  in fst . head $ dropWhile (uncurry (/=)) pairs

-- Generate all unique pairs from a list, avoiding duplicates (a,b) and (b,a)
uniquePairs :: Ord a => [a] -> [(a, a)]
uniquePairs xs = [(a, b) | a <- xs, b <- xs, a /= b, a < b]
