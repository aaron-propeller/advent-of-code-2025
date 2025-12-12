module AoCDays.Day10 (partA, partB) where
import AoCUtils.AoCParsing (splitOn, extractInts)
import System.Process (readProcess)
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isInfixOf)

type Input = [String]
type Output = Int

partA :: Input -> Output
partA input =
  let machines = map parseMachine input
      buttonPresses = map pressButtonsLights machines
  in sum buttonPresses

partB :: Input -> Output  
partB input =
  let machines = map parseMachine input
      -- Generate individual SMT files for each machine
      _ = unsafePerformIO $ do
            putStrLn $ "Generating SMT files for " ++ show (length machines) ++ " machines..."
            mapM_ generateMachineFile (zip [1..] machines)
            putStrLn "Done generating SMT files!"
  in length machines  -- Return number of machines processed


-- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}

type Lights = [Bool]
type LightsToToggle = [Int]
type Buttons = [LightsToToggle]
type Jolts = [Int]
-- Machine = (Current Lights, Wanted Lights, Buttons, Current Jolts, Wanted Jolts)
type Machine = (Lights, Lights, Buttons, Jolts, Jolts)

parseMachine :: String -> Machine 
parseMachine input = 
  let split = words input
      lights = head split
      buttons = init (tail split)
      jolts = last split
  in flattenTup (parseLights lights, parseButtons buttons, parseJolts jolts)

parseLights :: String -> (Lights, Lights) 
parseLights s = 
  let lights = init (tail s)
      iniitalLights = replicate (length lights) False
      currentLights = map (== '#') lights
  in (iniitalLights, currentLights)

parseButtons :: [String] -> Buttons 
parseButtons = map extractInts

parseJolts :: String -> (Jolts, Jolts)
parseJolts s = 
  let targetJolts = extractInts s
      initialJolts = replicate (length targetJolts) 0
  in (initialJolts, targetJolts)

flattenTup :: ((a, b), c, (d, e)) -> (a, b, c, d, e)
flattenTup ((a, b), c, (d, e)) = (a, b, c, d, e)

pressButtonsLights :: Machine -> Int
pressButtonsLights (machine@(currentLights, wantedLights, buttons, currentJolts, wantedJolts)) = 
    runButtonPressScenario wantedLights currentLights buttons 1


-- runButtonPressScenario CurrentLights TargetLights ButtonList MaxPresses PressesChosen
runButtonPressScenario :: Lights -> Lights -> Buttons -> Int -> Int 
runButtonPressScenario currentLights targetLights buttons maxPresses =
  let buttonCombinations = combinations maxPresses buttons
      -- for each combination of buttons, check if pressing them achieves the target lights 
      -- if any do, return MaxPresses
      -- else runButtonPressScenario with maxPresses + 1
      canAchieveTarget = any (achievesTarget currentLights targetLights) buttonCombinations
  in if canAchieveTarget
      then maxPresses
      else runButtonPressScenario currentLights targetLights buttons (maxPresses + 1)

achievesTarget :: Lights -> Lights -> Buttons -> Bool 
achievesTarget currentLights targetLights buttons =
  let finalLights = foldl toggleLights currentLights buttons
  in finalLights == targetLights 

toggleLights :: Lights -> LightsToToggle -> Lights 
toggleLights lights indices =
  foldl (\ls idx -> toggleLight ls idx) lights indices

toggleLight :: Lights -> Int -> Lights
toggleLight lights idx =
  let (before, light:after) = splitAt idx lights
  in before ++ (not light) : after

runButtonsJolts :: Machine -> Int 
runButtonsJolts (machine@(currentLights, wantedLights, buttons, currentJolts, wantedJolts)) = 
  solveButtonProblem wantedJolts buttons

runButtonPressScenarioJolts :: Jolts -> Jolts -> Buttons -> Int -> Int 
runButtonPressScenarioJolts currentJolts targetJolts buttons maxPresses =
  let buttonCombinations = combinations maxPresses buttons
      canAchieveTarget = any (achievesTargetJolts currentJolts targetJolts) buttonCombinations
  in if canAchieveTarget
      then maxPresses
      else runButtonPressScenarioJolts currentJolts targetJolts buttons (maxPresses + 1)

achievesTargetJolts :: Jolts -> Jolts -> Buttons -> Bool 
achievesTargetJolts currentJolts targetJolts buttons =
  let finalJolts = foldl addJolts currentJolts buttons
  in finalJolts == targetJolts 

addJolts :: Jolts -> LightsToToggle -> Jolts 
addJolts jolts indices =
  foldl (\js idx -> addJolt js idx) jolts indices 

addJolt :: Jolts -> Int -> Jolts
addJolt jolts idx =
  let (before, jolt:after) = splitAt idx jolts
  in before ++ (jolt + 1) : after

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs)
  | k > length (x:xs) || k <= 0 = [] -- Handle invalid k values if necessary
  | otherwise = (map (x :) (combinations (k - 1) xs)) ++ (combinations k xs)

solveButtonProblem :: [Int] -> [[Int]] -> Int
solveButtonProblem target buttons = 
    let machineId = 1  -- For simplicity, use ID 1
        smtCode = generateSMT machineId buttons target
    in case runZ3OnSMT smtCode of
         Just val -> val
         Nothing -> error ("No solution found. SMT code was:\n" ++ smtCode)

generateSMT :: Int -> [[Int]] -> [Int] -> String
generateSMT machineId buttons targets = 
    let numButtons = length buttons
        numPositions = length targets
        
        -- Declare variables
        declarations = unlines ["; Machine " ++ show machineId,
                               unlines ["(declare-const btn" ++ show machineId ++ "_" ++ show i ++ " Int)" 
                                       | i <- [0..numButtons-1]]]
        
        -- Non-negativity constraints
        nonNegConstraints = unlines ["(assert (>= btn" ++ show machineId ++ "_" ++ show i ++ " 0))" 
                                    | i <- [0..numButtons-1]]
        
        -- Position constraints
        positionConstraints = unlines [generatePositionConstraint machineId pos buttons (targets !! pos) 
                                      | pos <- [0..numPositions-1]]
        
        -- Objective
        buttonVars = ["btn" ++ show machineId ++ "_" ++ show i | i <- [0..numButtons-1]]
        objective = if numButtons == 1 
                   then "(minimize " ++ head buttonVars ++ ")"
                   else "(minimize (+ " ++ unwords buttonVars ++ "))"
        
        commands = unlines ["(check-sat)", "(get-model)"]
        
    in declarations ++ nonNegConstraints ++ positionConstraints ++ objective ++ "\n" ++ commands

generatePositionConstraint :: Int -> Int -> [[Int]] -> Int -> String
generatePositionConstraint machineId pos buttons targetVal = 
    let affectingButtons = [i | (i, buttonEffects) <- zip [0..] buttons, pos `elem` buttonEffects]
        buttonVars = ["btn" ++ show machineId ++ "_" ++ show i | i <- affectingButtons]
    in if null buttonVars
       then "(assert (= 0 " ++ show targetVal ++ "))"
       else if length buttonVars == 1
            then "(assert (= " ++ head buttonVars ++ " " ++ show targetVal ++ "))"
            else "(assert (= (+ " ++ unwords buttonVars ++ ") " ++ show targetVal ++ "))"

runZ3OnSMT :: String -> Maybe Int
runZ3OnSMT smtCode = unsafePerformIO $ do
    withSystemTempFile "machine.smt2" $ \path handle -> do
        hPutStr handle smtCode
        hClose handle
        output <- readProcess "z3" [path] ""
        return (extractObjectiveValue output)

generateMachineFile :: (Int, Machine) -> IO ()
generateMachineFile (machineId, machine@(currentLights, wantedLights, buttons, currentJolts, wantedJolts)) = do
    let filename = "machine_" ++ show machineId ++ ".smt2"
        header = "; Machine " ++ show machineId ++ " - " ++ show (length buttons) ++ " buttons, " ++ show (length wantedJolts) ++ " positions\n"
        smtCode = generateSMT machineId buttons wantedJolts
        content = header ++ smtCode
    writeFile filename content
    putStrLn $ "Generated " ++ filename

extractObjectiveValue :: String -> Maybe Int
extractObjectiveValue z3Output = 
    let outputLines = lines z3Output
    in if "sat" `elem` outputLines
       then let buttonValues = [read (last (words line)) | line <- outputLines,
                                "define-fun" `elem` words line,
                                "btn" `isInfixOf` line,
                                "Int" `elem` words line,
                                not (null (words line)),
                                let lastWord = last (words line),
                                all (\c -> c `elem` "0123456789") lastWord]
            in if null buttonValues then Nothing else Just (sum buttonValues)
       else Nothing
