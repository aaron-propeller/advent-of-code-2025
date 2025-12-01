import System.Environment (getArgs)

main :: IO ()
main = do 
  filename <- getArgs
  file <- readFile $ head filename
  let fileLines = lines file
  let rotations = fileLines
  let currentLocation = 50
  let startZeroCount = 0
  -- probably could reduce this with foldl but oh well
  let finalZeroCount = applyRotations currentLocation rotations startZeroCount
  print finalZeroCount

applyRotations :: Int -> [String] -> Int -> Int
applyRotations _ [] count = count
applyRotations currentLocation (nextLocation:remainingLocations) count =
  let (direction, magnitude) = parseLocation nextLocation
      -- 100 is a full rotation back to the same spot
      remainingMovement = magnitude `mod` 100 
      newLocation = normalizeLocation $ case direction of
                      "L" -> currentLocation - remainingMovement
                      "R" -> currentLocation + remainingMovement
      atZero = if newLocation == 0 && currentLocation /= 0
               then 1
               else 0

  in applyRotations newLocation remainingLocations (count + atZero)


parseLocation :: String -> (String, Int)
parseLocation location = 
  let direction = take 1 location
      magnitude = read (drop 1 location) :: Int
  in (direction, magnitude)

-- Normalize location to be within 0-99
normalizeLocation :: Int -> Int
normalizeLocation location
  | location < 0 = location + 100
  | location >= 100 = location - 100
  | otherwise = location
