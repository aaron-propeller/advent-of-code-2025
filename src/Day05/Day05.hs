module Day05.Day05 (partA, partB) where

import AoCUtils (splitOn, splitOnList)
import Data.List (sortBy)
import Data.Ord (comparing)

type Input = [String]
type Output = Int

type Range = (Int, Int)

partA :: Input -> Output
partA input = 
  let (fresh, available) = parseInput input
      availableAlsoFresh = filter (\n -> any (inRange n) fresh) available
  in length availableAlsoFresh

partB :: Input -> Output  
partB input = 
  let (fresh, _) = parseInput input 
      sortedFresh =  sortBy (comparing fst) fresh
      finalRanges = foldl mergeRanges [] sortedFresh
  in sum $ map (\(start, end) -> end - start + 1) finalRanges

parseInput :: Input -> ([Range], [Int])
parseInput input =
  case splitOnList "" input of
    (rangeLines:availableLines:_) -> 
      let fresh = map parseRange rangeLines
          available = map read availableLines :: [Int]
      in (fresh, available)
    [_] -> error "Invalid input"
    [] -> error "Empty input"

parseRange :: String -> Range 
parseRange line = 
  case splitOn '-' line of
    [startStr, endStr] -> 
      let start = read startStr :: Int
          end = read endStr :: Int
      in (start, end)
    _ -> error "Invalid range format"

inRange :: Int -> Range -> Bool
inRange n (start, end) = n >= start && n <= end

mergeRanges :: [Range] -> Range -> [Range]
mergeRanges [] newRange = [newRange]
mergeRanges acc@(lastRange@(start1, end1):rest) newRange@(start2, end2) =
  if rangesOverlap lastRange newRange then
    (min start1 start2, max end1 end2) : rest
  else
    newRange : acc

rangesOverlap :: Range -> Range -> Bool 
rangesOverlap (start1, end1) (start2, end2) =
  not (end1 < start2 || end2 < start1)
