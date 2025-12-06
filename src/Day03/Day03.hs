module Day03.Day03 (partA, partB) where

import Data.List (elemIndex)

type Input = [String]
type Output = Int

partA :: Input -> Output
partA input = 
  let banks = map (map (\c -> read[c] :: Int)) input
      jolts = map (`findJolts` 2) banks
      result = sum jolts
  in result

partB :: Input -> Output
partB input = 
  let banks = map (map (\c -> read[c] :: Int)) input
      jolts = map (`findJolts` 12) banks
      result = sum jolts
  in result

findJolts :: [Int] -> Int -> Int
findJolts _ 0 = 0
findJolts bank joltSize = 
  let nextJoltSize = joltSize - 1
      (front, _) = splitAt (length bank - nextJoltSize) bank
      largest = maximum front
      largestIndex = elemIndex largest bank
      (_, rest) = splitAt (maybe 0 (+1) largestIndex) bank
  in (largest * 10 ^ nextJoltSize + findJolts rest nextJoltSize)
