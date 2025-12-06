module AoCUtils.AoCList where

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