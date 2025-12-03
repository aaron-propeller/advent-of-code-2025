module Day02A (solve) where

import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))

type Input = [String]
type Output = Int

pairUp :: [a] -> [[a]]
pairUp [] = []
pairUp [_] = []
pairUp (x:y:xs) = [x,y] : pairUp xs

solve :: Input -> Output
solve input =
  let inputStr = head input
      extracted = map read (getAllTextMatches (inputStr =~ "[0-9]+")) :: [Int]
      numbers = map show $ concat [ [start..end] | [start, end] <- pairUp extracted ]
      repeats = filter hasRepeat numbers
  in sum $ map read repeats

hasRepeat :: String -> Bool
hasRepeat s = do
  let (first, second) = splitAt (length s `div` 2) s 
  first == second