module AoCRunner.AoCFiles where

import System.Directory (doesFileExist)
import AoCRunner.AoCDisplay (Expected(..))

type DayNumber = String

data FileType = Sample | Input | ExpectedFile
  deriving (Show, Eq)

-- Build path to input files
buildInputPath :: DayNumber -> FileType -> String
buildInputPath day fileType = "inputs/day" ++ day ++ "/" ++ fileTypeName fileType ++ ".txt"
  where
    fileTypeName Sample = "sample"
    fileTypeName Input = "input" 
    fileTypeName ExpectedFile = "expected"

-- Read input file and return lines
readInputFile :: DayNumber -> FileType -> IO [String]
readInputFile day fileType = do
  let filePath = buildInputPath day fileType
  content <- readFile filePath
  return $ lines content

-- Read expected results from file
readExpectedResults :: DayNumber -> IO Expected
readExpectedResults day = do
  let expectedFile = buildInputPath day ExpectedFile
  exists <- doesFileExist expectedFile
  if exists
    then do
      content <- readFile expectedFile
      let lns = lines content
      case lns of
        [a, b] -> return $ Expected (Just (read a)) (Just (read b))
        [a] -> return $ Expected (Just (read a)) Nothing
        _ -> return $ Expected Nothing Nothing
    else return $ Expected Nothing Nothing

-- Check if a file exists for a day and file type
fileExists :: DayNumber -> FileType -> IO Bool
fileExists day fileType = doesFileExist (buildInputPath day fileType)