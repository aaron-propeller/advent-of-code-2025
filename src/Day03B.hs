module Day03B (solve) where

import Data.List (elemIndex)

type Input = [String]
type Output = Int

solve :: Input -> Output
solve input = 
  let banks = map (map (\c -> read[c] :: Int)) input
      jolts = map (`findJolts` 12) banks
      result = sum jolts
  in result

findJolts :: [Int] -> Int -> Int
findJolts _ 0 = 0
findJolts bank joltSize = 
  let (front, _) = splitAt (length bank - (joltSize - 1)) bank
      largest = maximum front
      largestIndex = elemIndex largest bank
      (_, rest) = splitAt (maybe 0 (+1) largestIndex) bank
  in (largest * 10 ^ (joltSize - 1) + findJolts rest (joltSize - 1))