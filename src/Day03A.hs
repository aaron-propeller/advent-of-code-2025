module Day03A (solve) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

type Input = [String]
type Output = Int

solve :: Input -> Output
solve input = 
  let banks = map (map (\c -> read[c] :: Int)) input
      jolts = map findJolts banks
      result = sum jolts
  in result

findJolts :: [Int] -> Int 
findJolts bank = 
  let front = init bank 
      largest = maximum front
      largestIndex = elemIndex largest bank
      (_, rest) = splitAt (fromMaybe 0 largestIndex + 1) bank
      secondLargest = maximum rest
  in (largest * 10 + secondLargest)