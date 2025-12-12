#!/usr/bin/env runhaskell
import Data.Char (isDigit)

-- Extract all integers from a string
extractInts :: String -> [Int]
extractInts s = map read $ words $ map (\c -> if isDigit c then c else ' ') s 

main :: IO ()
main = do
    content <- readFile "all_z3_output.txt"
    
    putStrLn "Extracting all numbers from Z3 output..."
    
    let allNumbers = extractInts content
        totalSum = sum allNumbers
        numberCount = length allNumbers
    
    putStrLn $ "Found " ++ show numberCount ++ " numbers"
    putStrLn $ "Total sum: " ++ show totalSum