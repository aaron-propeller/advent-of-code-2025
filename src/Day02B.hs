module Day02B (solve) where

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
hasRepeat s = or $ [ checkSubstringsOfLength len s | len <- [1..(length s `div` 2)] ]

checkSubstringsOfLength :: Int -> String -> Bool
checkSubstringsOfLength len s =
  case splitIntoLengths len s of
    [] -> error "Empty list in checkSubstringsOfLength"
    (x:xs) -> all (== x) xs

splitIntoLengths :: Int -> String -> [String]
splitIntoLengths _ [] = []
splitIntoLengths n xs = take n xs : splitIntoLengths n (drop n xs)