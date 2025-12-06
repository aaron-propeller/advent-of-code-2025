module AoCDays.Day02 (partA, partB) where

import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))

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
      repeats = filter hasRepeatA numbers
  in sum $ map read repeats

partB :: Input -> Output
partB input =
  let inputStr = head input
      extracted = map read (getAllTextMatches (inputStr =~ "[0-9]+")) :: [Int]
      numbers = map show $ concat [ [start..end] | [start, end] <- pairUp extracted ]
      repeats = filter hasRepeatB numbers
  in sum $ map read repeats

hasRepeatA :: String -> Bool
hasRepeatA s = do
  let (first, second) = splitAt (length s `div` 2) s 
  first == second

hasRepeatB :: String -> Bool
hasRepeatB s = or $ [ checkSubstringsOfLength len s | len <- [1..(length s `div` 2)] ]

checkSubstringsOfLength :: Int -> String -> Bool
checkSubstringsOfLength len s =
  case  splitIntoLengths len s of
    [] -> error "Empty list in checkSubstringsOfLength"
    (x:xs) -> all (== x) xs

splitIntoLengths :: Int -> String -> [String]
splitIntoLengths _ [] = []
splitIntoLengths n xs = take n xs : splitIntoLengths n (drop n xs)
