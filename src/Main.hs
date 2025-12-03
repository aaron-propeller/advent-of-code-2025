import System.Environment (getArgs)
import System.Exit (die)
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Exception (evaluate)
import Text.Printf (printf)
import qualified Day01A
import qualified Day01B
import qualified Day02A
import qualified Day02B
import qualified Day03A
import qualified Day03B
import qualified Day04A
import qualified Day04B
import qualified Day05A
import qualified Day05B
import qualified Day06A
import qualified Day06B
import qualified Day07A
import qualified Day07B
import qualified Day08A
import qualified Day08B
import qualified Day09A
import qualified Day09B
import qualified Day10A
import qualified Day10B
import qualified Day11A
import qualified Day11B
import qualified Day12A
import qualified Day12B

type Solver = [String] -> Int

solvers :: Map.Map String Solver
solvers = Map.fromList
    [ ("01a", Day01A.solve)
    , ("01b", Day01B.solve)
    , ("02a", Day02A.solve)
    , ("02b", Day02B.solve)
    , ("03a", Day03A.solve)
    , ("03b", Day03B.solve)
    , ("04a", Day04A.solve)
    , ("04b", Day04B.solve)
    , ("05a", Day05A.solve)
    , ("05b", Day05B.solve)
    , ("06a", Day06A.solve)
    , ("06b", Day06B.solve)
    , ("07a", Day07A.solve)
    , ("07b", Day07B.solve)
    , ("08a", Day08A.solve)
    , ("08b", Day08B.solve)
    , ("09a", Day09A.solve)
    , ("09b", Day09B.solve)
    , ("10a", Day10A.solve)
    , ("10b", Day10B.solve)
    , ("11a", Day11A.solve)
    , ("11b", Day11B.solve)
    , ("12a", Day12A.solve)
    , ("12b", Day12B.solve)
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day, inputFile] -> runDay day inputFile
        _ -> usage

usage :: IO ()
usage = die $ unlines
    [ "Usage: aoc <day> <input-file>"
    , ""
    , "Examples:"
    , "  aoc 1a input.txt"
    , "  aoc 2b sample.txt"
    , ""
    , "Available days: " ++ unwords (Map.keys solvers)
    ]

runDay :: String -> String -> IO ()
runDay dayStr inputFile = do
    content <- readFile inputFile
    let day = map toLower dayStr
    case Map.lookup day solvers of
        Just solver -> do
            putStrLn $ "Running " ++ day ++ " with input from " ++ inputFile
            
            -- Time the parsing
            parseStart <- getCurrentTime
            let input = lines content
            _ <- evaluate (length input)  -- Force evaluation of input parsing
            parseEnd <- getCurrentTime
            let parseTime = diffUTCTime parseEnd parseStart
            
            -- Time the solver
            solveStart <- getCurrentTime
            let result = solver input
            _ <- evaluate result  -- Force evaluation of result
            solveEnd <- getCurrentTime
            let solveTime = diffUTCTime solveEnd solveStart
            
            -- Display results
            putStrLn $ "Result: " ++ show result
            printf "Parse time: %.6f ms\n" (realToFrac parseTime * 1000 :: Double)
            printf "Solve time: %.6f ms\n" (realToFrac solveTime * 1000 :: Double)
            printf "Total time: %.6f ms\n" (realToFrac (parseTime + solveTime) * 1000 :: Double)
            
        Nothing     -> die $ "Unknown day: " ++ dayStr ++ ". Run without arguments for usage."