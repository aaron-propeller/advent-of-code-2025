import System.Environment (getArgs)
import AoCUtils

main :: IO ()
main = do 
  [filename] <- getArgs
  content <- readFile filename
  print $ solve $ lines content

-- Puzzle solution below

type Input = [String]
type Output = Int

solve :: Input -> Output
solve input = 0
