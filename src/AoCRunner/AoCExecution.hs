module AoCRunner.AoCExecution where

import Data.Time (getCurrentTime, diffUTCTime)
import Control.Exception (evaluate)
import Text.Printf (printf)
import AoCRunner.AoCDisplay (Result(..), Timing(..), Expected(..), displayDayResults)
import AoCRunner.AoCFiles (DayNumber, FileType(..), readInputFile, readExpectedResults)
import AoCRunner.AoCRegistry (DayRunner, getDayRunner)

-- Execute a day runner with timing
executeDay :: DayRunner -> [String] -> IO (Result, Timing)
executeDay dayRunner input = do
  -- Time the parsing
  parseStart <- getCurrentTime
  _ <- evaluate (length input)  -- Force evaluation of input parsing
  parseEnd <- getCurrentTime
  let parseDuration = diffUTCTime parseEnd parseStart
  
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
  
  let result = Result resultA resultB
  let timing = Timing parseDuration solveTimeA solveTimeB
  return (result, timing)

-- Execute sample only for a day
executeDaySample :: DayNumber -> IO ()
executeDaySample day = do
  case getDayRunner day of
    Nothing -> error $ "Unknown day: " ++ day
    Just dayRunner -> do
      -- Read expected results and sample input
      expected <- readExpectedResults day
      sampleInput <- readInputFile day SampleFile
      (sampleResult, sampleTiming) <- executeDay dayRunner sampleInput
      
      -- Display sample results with expected comparison
      putStrLn $ "=== Day " ++ day ++ " (Sample Only) ==="
      displayDayResults day sampleResult sampleTiming expected sampleResult sampleTiming

-- Execute both sample and input for a day with enhanced display
executeDayBoth :: DayNumber -> IO ()
executeDayBoth day = do
  case getDayRunner day of
    Nothing -> error $ "Unknown day: " ++ day
    Just dayRunner -> do
      -- Read expected results
      expected <- readExpectedResults day
      
      -- Run sample and input
      sampleInput <- readInputFile day SampleFile
      inputInput <- readInputFile day InputFile
      
      (sampleResult, sampleTiming) <- executeDay dayRunner sampleInput
      (inputResult, inputTiming) <- executeDay dayRunner inputInput
      
      -- Display enhanced results
      displayDayResults day sampleResult sampleTiming expected inputResult inputTiming

-- Execute a single day with specific input file (legacy mode)
executeDaySingle :: DayNumber -> String -> IO ()
executeDaySingle day inputFile = do
  case getDayRunner day of
    Nothing -> error $ "Unknown day: " ++ day
    Just dayRunner -> do
      content <- readFile inputFile
      putStrLn $ "Running day " ++ day ++ " with input from " ++ inputFile
      
      (result, timing) <- executeDay dayRunner (lines content)
      
      let totalTime = parseTime timing + partATime timing + partBTime timing
      
      -- Display results in legacy format
      printf "Parse time: %.6f ms\n" (realToFrac (parseTime timing) * 1000 :: Double)
      putStrLn $ "Part A: " ++ show (partA result)
      printf "Part A time: %.6f ms\n" (realToFrac (partATime timing) * 1000 :: Double)
      putStrLn $ "Part B: " ++ show (partB result)
      printf "Part B time: %.6f ms\n" (realToFrac (partBTime timing) * 1000 :: Double)
      printf "Total time: %.6f ms\n" (realToFrac totalTime * 1000 :: Double)