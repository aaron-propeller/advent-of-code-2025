import System.Environment (getArgs)
import System.Exit (die)
import Data.Char (toLower, isDigit)
import qualified Data.Map as Map
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Exception (evaluate)
import Control.Monad (when)
import Text.Printf (printf)
import qualified Day01.Day01 as Day01
import qualified Day02.Day02 as Day02
import qualified Day03.Day03 as Day03
import qualified Day04.Day04 as Day04
import qualified Day05.Day05 as Day05
import qualified Day06.Day06 as Day06
import qualified Day07.Day07 as Day07
import qualified Day08.Day08 as Day08
import qualified Day09.Day09 as Day09
import qualified Day10.Day10 as Day10
import qualified Day11.Day11 as Day11
import qualified Day12.Day12 as Day12

type DayRunner = [String] -> (Int, Int)

dayRunners :: Map.Map String DayRunner
dayRunners = Map.fromList
    [ ("01", \input -> (Day01.partA input, Day01.partB input))
    , ("02", \input -> (Day02.partA input, Day02.partB input))
    , ("03", \input -> (Day03.partA input, Day03.partB input))
    , ("04", \input -> (Day04.partA input, Day04.partB input))
    , ("05", \input -> (Day05.partA input, Day05.partB input))
    , ("06", \input -> (Day06.partA input, Day06.partB input))
    , ("07", \input -> (Day07.partA input, Day07.partB input))
    , ("08", \input -> (Day08.partA input, Day08.partB input))
    , ("09", \input -> (Day09.partA input, Day09.partB input))
    , ("10", \input -> (Day10.partA input, Day10.partB input))
    , ("11", \input -> (Day11.partA input, Day11.partB input))
    , ("12", \input -> (Day12.partA input, Day12.partB input))
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day] -> runDayBoth day  -- Run both sample and input
        [day, inputFile] -> runDay day inputFile
        _ -> usage

usage :: IO ()
usage = die $ unlines
    [ "Usage: aoc <day> [input-file]"
    , ""
    , "Examples:"
    , "  aoc 1              # Runs both Day01/sample.txt and Day01/input.txt"
    , "  aoc 4 custom.txt   # Uses custom file path"
    , ""
    , "Available days: " ++ unwords (Map.keys dayRunners)
    ]

validateDayFormat :: String -> IO String
validateDayFormat dayStr = do
    let day = map toLower dayStr
    
    -- Check format (should be 1-2 digits)
    when (not (all isDigit day) || null day || length day > 2) $
        die $ "Invalid day format: " ++ dayStr ++ ". Expected 1-2 digits (e.g., 1, 04, 12)"
    
    -- Normalize to 2 digits and check range
    let dayNum = read day :: Int
    when (dayNum < 1 || dayNum > 25) $
        die $ "Day must be between 1 and 25, got: " ++ show dayNum
    return $ printf "%02d" dayNum

runDay :: String -> String -> IO ()
runDay dayStr inputFile = do
    content <- readFile inputFile
    day <- validateDayFormat dayStr
    case Map.lookup day dayRunners of
        Just dayRunner -> do
            putStrLn $ "Running day " ++ day ++ " with input from " ++ inputFile
            
            -- Time the parsing
            parseStart <- getCurrentTime
            let input = lines content
            _ <- evaluate (length input)  -- Force evaluation of input parsing
            parseEnd <- getCurrentTime
            let parseTime = diffUTCTime parseEnd parseStart
            
            -- Time part A
            solveStartA <- getCurrentTime
            let (partARunner, partBRunner) = dayRunner input
            resultA <- evaluate partARunner  -- Force evaluation of part A
            solveEndA <- getCurrentTime
            let solveTimeA = diffUTCTime solveEndA solveStartA
            
            -- Time part B
            solveStartB <- getCurrentTime
            resultB <- evaluate partBRunner  -- Force evaluation of part B
            solveEndB <- getCurrentTime
            let solveTimeB = diffUTCTime solveEndB solveStartB
            
            let totalSolveTime = solveTimeA + solveTimeB
            
            -- Display results
            printf "Parse time: %.6f ms\n" (realToFrac parseTime * 1000 :: Double)
            putStrLn $ "Part A: " ++ show resultA
            printf "Part A time: %.6f ms\n" (realToFrac solveTimeA * 1000 :: Double)
            putStrLn $ "Part B: " ++ show resultB
            printf "Part B time: %.6f ms\n" (realToFrac solveTimeB * 1000 :: Double)
            printf "Total time: %.6f ms\n" (realToFrac (parseTime + totalSolveTime) * 1000 :: Double)
            
        Nothing     -> die $ "Unknown day: " ++ dayStr ++ ". Run without arguments for usage."

buildInputPath :: String -> String -> String
buildInputPath day fileType = "src/Day" ++ day ++ "/" ++ fileType ++ ".txt"

runDayBoth :: String -> IO ()
runDayBoth dayStr = do
    day <- validateDayFormat dayStr
    let sampleFile = buildInputPath day "sample"
    let inputFile = buildInputPath day "input"
    
    putStrLn $ "=== Running Day " ++ day ++ " with sample.txt ==="
    runDay dayStr sampleFile
    putStrLn ""
    putStrLn $ "=== Running Day " ++ day ++ " with input.txt ==="
    runDay dayStr inputFile
