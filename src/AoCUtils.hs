module AoCUtils where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Map as Map

-- Common type aliases
type Coord = (Int, Int)
type Grid = Map.Map Coord Char
data Direction = North | South | East | West deriving (Show, Eq, Ord, Enum)

-- Common parsing utilities
parseNumbers :: String -> [Int]
parseNumbers = mapMaybe readMaybe . words

parseInt :: String -> Maybe Int
parseInt = readMaybe

-- Split on any character
splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs
        f _ [] = []

-- Grid operations
parseGrid :: [String] -> Grid
parseGrid rows = Map.fromList 
  [((x, y), c) | (y, row) <- zip [0..] rows
               , (x, c) <- zip [0..] row]

neighbors4 :: Coord -> [Coord]
neighbors4 (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

neighbors8 :: Coord -> [Coord] 
neighbors8 (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

-- Direction helpers
moveCoord :: Direction -> Coord -> Coord
moveCoord North (x, y) = (x, y-1)
moveCoord South (x, y) = (x, y+1)
moveCoord East (x, y) = (x+1, y)
moveCoord West (x, y) = (x-1, y)

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East  
turnLeft East = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

-- Common utility functions
count :: (a -> Bool) -> [a] -> Int
count predicate = length . filter predicate

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- Math utilities
manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- List utilities
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

windows :: Int -> [a] -> [[a]]
windows n xs = map (take n) (take (length xs - n + 1) (iterate tail xs))