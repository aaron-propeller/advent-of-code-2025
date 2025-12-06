module AoCDays.Day05 (partA, partB) where

import AoCUtils.AoCParsing (splitAtSep)
import AoCUtils.AoCRange (Range, parseRange, inRange, mergeRanges)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)

type Input = [String]
type Output = Int

partA :: Input -> Output
partA input = 
  let (fresh, available) = parseInput input
      availableAlsoFresh = filter (\n -> any (inRange n) fresh) available
  in length availableAlsoFresh

partB :: Input -> Output  
partB input = 
  let (fresh, _) = parseInput input 
      sortedFresh =  sortBy (comparing fst) fresh
      finalRanges = mergeSortedRanges sortedFresh
  in sum $ map (\(start, end) -> end - start + 1) finalRanges

parseInput :: Input -> ([Range], [Int])
parseInput input =
  let (rangeLines, availableLines) = splitAtSep "" input
      fresh = mapMaybe parseRange rangeLines
      available = map read availableLines :: [Int]
  in (fresh, available)

mergeSortedRanges :: [Range] -> [Range]
mergeSortedRanges [] = []
mergeSortedRanges [r] = [r]
mergeSortedRanges (r1:r2:rest) =
  case mergeRanges r1 r2 of
    Just merged -> mergeSortedRanges (merged:rest)
    Nothing -> r1 : mergeSortedRanges (r2:rest)
