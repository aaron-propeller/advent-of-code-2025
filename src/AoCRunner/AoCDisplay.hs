module AoCRunner.AoCDisplay where

import Text.Printf (printf)
import Data.Time (NominalDiffTime)

data Result = Result
  { partA :: Int
  , partB :: Int
  } deriving (Show, Eq)

data Timing = Timing
  { parseTime :: NominalDiffTime
  , partATime :: NominalDiffTime
  , partBTime :: NominalDiffTime
  } deriving (Show)

data Expected = Expected
  { expectedA :: Maybe Int
  , expectedB :: Maybe Int
  } deriving (Show)

-- Performance indicators based on timing
performanceIndicator :: NominalDiffTime -> String
performanceIndicator time
  | timeMs < 10    = "🚀"
  | timeMs < 100   = "⚡"
  | timeMs >= 1000 = "🐌"
  | otherwise      = ""
  where
    timeMs = realToFrac time * 1000 :: Double

-- Format timing in milliseconds
formatTime :: NominalDiffTime -> String
formatTime time = printf "%.6f ms" (realToFrac time * 1000 :: Double)

-- Check if result matches expected
checkResult :: Int -> Maybe Int -> (String, Bool)
checkResult actual Nothing = (show actual, True)  -- No expected value, assume correct
checkResult actual (Just expected)
  | actual == expected = (show actual ++ " ✅ (Expected: " ++ show expected ++ ")", True)
  | otherwise = (show actual ++ " ❌ (Expected: " ++ show expected ++ ")", False)

-- Display sample results with expected comparison
displaySampleResults :: Result -> Timing -> Expected -> IO Bool
displaySampleResults result timing expected = do
  putStrLn "📋 SAMPLE:"
  
  let (partAStr, partACorrect) = checkResult (partA result) (expectedA expected)
  let (partBStr, partBCorrect) = checkResult (partB result) (expectedB expected)
  
  putStrLn $ "   Part A: " ++ partAStr ++ " [" ++ formatTime (partATime timing) ++ "] " ++ performanceIndicator (partATime timing)
  putStrLn $ "   Part B: " ++ partBStr ++ " [" ++ formatTime (partBTime timing) ++ "] " ++ performanceIndicator (partBTime timing)
  
  let allCorrect = partACorrect && partBCorrect
  return allCorrect

-- Display input results
displayInputResults :: Result -> Timing -> IO ()
displayInputResults result timing = do
  putStrLn ""
  putStrLn "🎯 INPUT:"
  putStrLn $ "   Part A: " ++ show (partA result) ++ " [" ++ formatTime (partATime timing) ++ "] " ++ performanceIndicator (partATime timing)
  putStrLn $ "   Part B: " ++ show (partB result) ++ " [" ++ formatTime (partBTime timing) ++ "] " ++ performanceIndicator (partBTime timing)

-- Display timing summary
displayTimingSummary :: Timing -> Timing -> IO ()
displayTimingSummary sampleTiming inputTiming = do
  let avgParseTime = (parseTime sampleTiming + parseTime inputTiming) / 2
  let avgPartATime = (partATime sampleTiming + partATime inputTiming) / 2
  let avgPartBTime = (partBTime sampleTiming + partBTime inputTiming) / 2
  let totalTime = parseTime sampleTiming + partATime sampleTiming + partBTime sampleTiming +
                  parseTime inputTiming + partATime inputTiming + partBTime inputTiming
  
  putStrLn ""
  putStrLn $ "Parse time: " ++ formatTime avgParseTime
  putStrLn $ "Part A time: " ++ formatTime avgPartATime ++ " (avg)"
  putStrLn $ "Part B time: " ++ formatTime avgPartBTime ++ " (avg)"
  putStrLn $ "Total time: " ++ formatTime totalTime

-- Display enhanced results for a day
displayDayResults :: String -> Result -> Timing -> Expected -> Result -> Timing -> IO ()
displayDayResults day sampleResult sampleTiming expected inputResult inputTiming = do
  putStrLn $ "=== Day " ++ day ++ " Results ==="
  putStrLn ""
  
  sampleCorrect <- displaySampleResults sampleResult sampleTiming expected
  displayInputResults inputResult inputTiming
  
  unless sampleCorrect $ do
    putStrLn ""
    putStrLn "Sample mismatch detected! Check your implementation."
  
  displayTimingSummary sampleTiming inputTiming
  where
    unless True _ = return ()
    unless False action = action