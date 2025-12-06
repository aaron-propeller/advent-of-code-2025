module AoCDays.Day06-readable (partA, partB) where
import Data.List (transpose, groupBy)
import AoCUtils.AoCParsing (parseNumbers)
import Control.Arrow ((&&&))

type Input = [String]
type Output = Int

partA :: Input -> Output
partA = answerHomework . uncurry zip . (parseSymbols &&& parseA)

partB :: Input -> Output 
partB = answerHomework . uncurry zip . (parseSymbols &&& parseB)

answerHomework :: [(String, [Int])] -> Int
answerHomework = sum . map applyOperation

parseSymbols :: Input -> [String]
parseSymbols = words . last

parseA :: Input -> [[Int]]
parseA = transpose . map parseNumbers . init

parseB :: Input -> [[Int]]
parseB = map (concatMap parseNumbers) . splitOnSpaces . transpose . init

applyOperation :: (String, [Int]) -> Int 
applyOperation (symbol, values) = case symbol of
  "+" -> sum values
  "*" -> product values
  _ -> error "Unknown operation"

splitOnSpaces :: [String] -> [[String]]
splitOnSpaces = map (filter (not . all (== ' '))) .
                  groupBy (\_ y -> not (all (== ' ') y))

