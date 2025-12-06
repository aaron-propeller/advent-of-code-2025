module AoCDays.Day06 (partA, partB) where
import Data.List (transpose)
import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))

type Input = [String]
type Output = Int

partA :: Input -> Output
partA input = 
  let (matrix, symbols) = splitAt (length input - 1) input
      parsedMatrix = map extractNumbers matrix
      columns = transpose parsedMatrix
      symbolsWithValues = zip (words $ head symbols) columns
  in sum $ map applyOperation symbolsWithValues

partB :: Input -> Output  
partB input = 
  let (matrix, symbols) = splitAt (length input - 1) input
      columns = transpose matrix 
      groupedColumns = splitOnSpaces columns
      parsedColumns = map (concatMap extractNumbers) groupedColumns
      symbolsWithValues = zip (words $ head symbols) parsedColumns
  in sum $ map applyOperation symbolsWithValues

extractNumbers :: String -> [Int]
extractNumbers input = map read $ getAllTextMatches (input =~ "[0-9]+")

applyOperation :: (String, [Int]) -> Int 
applyOperation (symbol, values) = case symbol of
  "+" -> sum values
  "*" -> product values
  _ -> error "Unknown operation"

splitOnSpaces :: [String] -> [[String]]
splitOnSpaces xs = case break (all (== ' ')) xs of
  (chunk, []) -> [chunk | not (null chunk)]
  (chunk, _:rest) -> [chunk | not (null chunk)] ++ splitOnSpaces rest
