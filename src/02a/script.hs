import System.Environment (getArgs)
import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))

main :: IO ()
main = do 
  [filename] <- getArgs
  content <- readFile filename
  print $ solve $ head $ lines content

-- Puzzle solution below

type Input = String
type Output = Int

pairUp :: [a] -> [[a]]
pairUp [] = []
pairUp [_] = []
pairUp (x:y:xs) = [x,y] : pairUp xs

solve :: Input -> Output
solve input = 
  let extracted = map read (getAllTextMatches (input =~ "[0-9]+")) :: [Int]
      numbers = map show $ concat [ [start..end] | [start, end] <- pairUp extracted ]
      repeats = filter hasRepeat numbers
  in sum $ map read repeats

hasRepeat :: String -> Bool
hasRepeat s = do
  let (first, last) = splitAt (length s `div` 2) s 
  first == last

