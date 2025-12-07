module AoCUtils.AoCList (
    count, safeHead, safeTail,
    chunks, windows, pairUp, freqMap
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

-- Pair up elements into tuples, error if odd number
pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp [_] = error "pairUp: odd number of elements"
pairUp (x:y:xs) = (x, y) : pairUp xs

-- Create frequency map from list
freqMap :: Ord a => [a] -> Map.Map a Int
freqMap = Map.fromListWith (+) . map (\x -> (x, 1))
