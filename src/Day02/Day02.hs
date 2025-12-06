module Day02.Day02 (partA, partB) where

import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))
import AoCUtils (chunks)

type Input = [String]
type Output = Int

pairUp :: [a] -> [[a]]
pairUp [] = []
pairUp [_] = []
pairUp (x:y:xs) = [x,y] : pairUp xs

partA :: Input -> Output
partA input =
  let inputStr = head input
      extracted = map read (getAllTextMatches (inputStr =~ "[0-9]+")) :: [Int]
      numbers = map show $ concat [ [start..end] | [start, end] <- pairUp extracted ]
      strLen = length inputStr
      repeats = filter (hasRepeat (strLen `div` 2)) numbers
  in sum $ map read repeats

partB :: Input -> Output
partB input =
  let inputStr = head input
      extracted = map read (getAllTextMatches (inputStr =~ "[0-9]+")) :: [Int]
      numbers = map show $ concat [ [start..end] | [start, end] <- pairUp extracted ]
      repeats = filter (hasRepeat 1) numbers
  in sum $ map read repeats

hasRepeat :: Int -> String -> Bool
hasRepeat minLen s = or $ [ checkSubstringsOfLength len s | len <- [minLen..(length s `div` 2)] ]

checkSubstringsOfLength :: Int -> String -> Bool
checkSubstringsOfLength len s =
  case chunks len s of
    [] -> error "Empty list in checkSubstringsOfLength"
    (x:xs) -> all (== x) xs
