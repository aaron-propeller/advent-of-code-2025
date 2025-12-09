module AoCRunner.AoCCLI where

import System.Exit (die)
import Data.Char (toLower, isDigit)
import Control.Monad (when)
import Text.Printf (printf)

type DayNumber = String

data AoCCommand 
  = RunBoth DayNumber           -- Run both sample and input for a day
  | RunSample DayNumber         -- Run sample only for a day
  | RunSingle DayNumber String  -- Run specific day with custom input file
  deriving (Show, Eq)

-- Parse command line arguments
parseArgs :: [String] -> [String] -> Either String AoCCommand
parseArgs availableDays args = case args of
  [day] -> do
    validDay <- validateDayFormat availableDays day
    return $ RunBoth validDay
  [day, "sample"] -> do
    validDay <- validateDayFormat availableDays day
    return $ RunSample validDay
  [day, inputFile] -> do
    validDay <- validateDayFormat availableDays day
    return $ RunSingle validDay inputFile
  _ -> Left usageMessage

-- Validate day format and range
validateDayFormat :: [String] -> String -> Either String DayNumber
validateDayFormat availableDays dayStr = do
  let day = map toLower dayStr
  
  -- Check format (should be 1-2 digits)
  when (not (all isDigit day) || null day || length day > 2) $
    Left $ "Invalid day format: " ++ dayStr ++ ". Expected 1-2 digits (e.g., 1, 04, 12)"
  
  -- Normalize to 2 digits and check range
  let dayNum = read day :: Int
  when (dayNum < 1 || dayNum > 25) $
    Left $ "Day must be between 1 and 25, got: " ++ show dayNum
  
  let normalizedDay = printf "%02d" dayNum
  
  -- Check if day is available
  when (normalizedDay `notElem` availableDays) $
    Left $ "Unknown day: " ++ dayStr ++ ". Available days: " ++ unwords availableDays
  
  return normalizedDay

-- Usage message
usageMessage :: String
usageMessage = unlines
  [ "Usage: aoc <day> [sample|input-file]"
  , ""
  , "Examples:"
  , "  aoc 1              # Runs both sample and input for Day 1"
  , "  aoc 1 sample       # Runs only sample for Day 1"
  , "  aoc 4 custom.txt   # Uses custom file path for Day 4"
  ]

-- Display usage and exit
showUsage :: [String] -> IO a
showUsage availableDays = die $ usageMessage ++ "\nAvailable days: " ++ unwords availableDays