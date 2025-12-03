import System.Environment (getArgs)
import System.CPUTime
import Text.Printf
import Debug.Trace
import Data.List (elemIndex)

main :: IO ()
main = do 
  [filename] <- getArgs
  content <- readFile filename
  
  start <- getCPUTime
  let result = solve $ lines content
  end <- getCPUTime
  
  let diff = fromIntegral (end - start) / (10^12)
      diffMicros = fromIntegral (end - start) / (10^6)
  printf "Result: %s\n" (show result)
  if diff < 0.001
    then printf "Execution time: %.0f microseconds\n" (diffMicros :: Double)
    else printf "Execution time: %.6f seconds\n" (diff :: Double)

-- Puzzle solution below

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
