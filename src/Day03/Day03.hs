module Day03.Day03 (partA, partB) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

type Input = [String]
type Output = Int

partA :: Input -> Output
partA input = 
  let banks = map (map (\c -> read[c] :: Int)) input
      jolts = map findJoltsA banks
      result = sum jolts
  in result

partB :: Input -> Output
partB input = 
  let banks = map (map (\c -> read[c] :: Int)) input
      jolts = map (`findJoltsB` 12) banks
      result = sum jolts
  in result

findJoltsA :: [Int] -> Int 
findJoltsA bank = 
  let front = init bank 
      largest = maximum front
      largestIndex = elemIndex largest bank
      (_, rest) = splitAt (fromMaybe 0 largestIndex + 1) bank
      secondLargest = maximum rest
  in (largest * 10 + secondLargest)

findJoltsB :: [Int] -> Int -> Int
findJoltsB _ 0 = 0
findJoltsB bank joltSize = 
  let (front, _) = splitAt (length bank - (joltSize - 1)) bank
      largest = maximum front
      largestIndex = elemIndex largest bank
      (_, rest) = splitAt (maybe 0 (+1) largestIndex) bank
  in (largest * 10 ^ (joltSize - 1) + findJoltsB rest (joltSize - 1))