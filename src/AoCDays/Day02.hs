module AoCDays.Day02 (partA, partB) where

import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))
import AoCUtils.AoCTuple (pairUp)

type Input = [String]
type Output = Int

partA :: Input -> Output
partA = sumRepeatsA . expandRanges . extractNumbers

partB :: Input -> Output
partB = sumRepeatsB . expandRanges . extractNumbers

extractNumbers :: Input -> [Int]
extractNumbers = map read . getAllTextMatches . (=~ "[0-9]+") . head

expandRanges :: [Int] -> [String]
expandRanges = map show . concatMap (\(start, end) -> [start..end]) . pairUp

sumRepeatsA :: [String] -> Int
sumRepeatsA = sum . map read . filter hasRepeatA

sumRepeatsB :: [String] -> Int
sumRepeatsB = sum . map read . filter hasRepeatB

hasRepeatA :: String -> Bool
hasRepeatA s = uncurry (==) $ splitAt (length s `div` 2) s

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
