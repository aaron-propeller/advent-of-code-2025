import System.Environment (getArgs)
import System.CPUTime
import Text.Printf

main :: IO ()
main = do 
  [filename] <- getArgs
  content <- readFile filename
  
  start <- getCPUTime
  let result = solve $ lines content
  end <- getCPUTime
  
  let diff = fromIntegral (end - start) / (10^12)
  printf "Result: %s\n" (show result)
  printf "Execution time: %.3f seconds\n" (diff :: Double)

-- Puzzle solution below

type Input = [String]
type Output = Int

solve :: Input -> Output
solve input = 0
