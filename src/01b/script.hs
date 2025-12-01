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
      -- Each full rotation counts as passing zero once
      fullRotations = magnitude `div` 100
      remainingMovement = magnitude `mod` 100 
      newLocation = case direction of
                      "L" -> currentLocation - remainingMovement
                      "R" -> currentLocation + remainingMovement
      (normalizedLocation, isNormalized) = normalizeLocation newLocation
      -- If we did a normalization and we were not on zero before, we must have crossed zero or be on zero
      isNormalizedZero = if currentLocation /= 0 && isNormalized == 1 then 1 else 0
      -- If we have ended up on zero and we were not on zero before it means the last rotation wasnt counted
      -- Normalization now has it's own rules, so this is just the remaining case
      atZero = if isNormalized /= 1 && normalizedLocation == 0 && currentLocation /= 0
               then 1
               else 0
      -- Sum the cases
      zeros = fullRotations + atZero + isNormalizedZero
  in applyRotations normalizedLocation remainingLocations (count + zeros)


parseLocation :: String -> (String, Int)
parseLocation location = 
  let direction = take 1 location
      magnitude = read (drop 1 location) :: Int
  in (direction, magnitude)

-- Normalize location to be within 0-99, include if we did anything
normalizeLocation :: Int -> (Int, Int)
normalizeLocation location
  | location < 0 = (location + 100, 1)
  | location >= 100 = (location - 100, 1)
  | otherwise = (location, 0)
