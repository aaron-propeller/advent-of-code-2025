#!/usr/bin/env runhaskell
import Data.List (isInfixOf)
import Data.Char (isDigit, chr, ord)

-- Simple parsing functions
extractInts :: String -> [Int]
extractInts s = map read $ words $ map (\c -> if isDigit c then c else ' ') s 

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

-- Extract jolts and buttons from parsed machine for SMT generation
extractMachineData :: Machine -> ([Int], [[Int]])
extractMachineData (_, _, buttons, _, targetJolts) = (targetJolts, buttons)

-- Convert button index to letter-based name
indexToLetter :: Int -> String
indexToLetter i = [chr (ord 'a' + i)]

-- Convert machine ID to letter-based name
machineIdToLetter :: Int -> String
machineIdToLetter i = "m" ++ [chr (ord 'a' + ((i - 1) `mod` 26))]

generateSMT :: Int -> [[Int]] -> [Int] -> String
generateSMT machineId buttons targets = 
    let numButtons = length buttons
        numPositions = length targets
        machineChar = machineIdToLetter machineId
        
        -- Declare variables with letter names
        declarations = unlines ["(declare-const btn" ++ machineChar ++ "_" ++ indexToLetter i ++ " Int)" 
                               | i <- [0..numButtons-1]]
        
        -- Non-negativity constraints
        nonNegConstraints = unlines ["(assert (>= btn" ++ machineChar ++ "_" ++ indexToLetter i ++ " 0))" 
                                    | i <- [0..numButtons-1]]
        
        -- Position constraints
        positionConstraints = unlines [generatePositionConstraint machineId pos buttons (targets !! pos) 
                                      | pos <- [0..numPositions-1]]
        
        -- Objective
        buttonVars = ["btn" ++ machineChar ++ "_" ++ indexToLetter i | i <- [0..numButtons-1]]
        objective = if numButtons == 1 
                   then "(minimize " ++ head buttonVars ++ ")"
                   else "(minimize (+ " ++ unwords buttonVars ++ "))"
        
        commands = unlines ["(check-sat)", "(get-model)"]
        
    in declarations ++ nonNegConstraints ++ positionConstraints ++ objective ++ "\n" ++ commands

generatePositionConstraint :: Int -> Int -> [[Int]] -> Int -> String
generatePositionConstraint machineId pos buttons targetVal = 
    let affectingButtons = [i | (i, buttonEffects) <- zip [0..] buttons, pos `elem` buttonEffects]
        machineChar = machineIdToLetter machineId
        buttonVars = ["btn" ++ machineChar ++ "_" ++ indexToLetter i | i <- affectingButtons]
    in if null buttonVars
       then "(assert (= 0 " ++ show targetVal ++ "))"
       else if length buttonVars == 1
            then "(assert (= " ++ head buttonVars ++ " " ++ show targetVal ++ "))"
            else "(assert (= (+ " ++ unwords buttonVars ++ ") " ++ show targetVal ++ "))"

main :: IO ()
main = do
    content <- readFile "inputs/day10/input.txt"
    let linesOfInput = filter (not . null) (lines content)
        machines = map parseMachine linesOfInput
        machineData = map extractMachineData machines
    
    putStrLn $ "Generating SMT files for " ++ show (length machines) ++ " machines..."
    
    mapM_ (\(i, (targets, buttons)) -> do
        let machineId = i
            filename = "machine_" ++ show machineId ++ ".smt2"
            header = "; Machine " ++ show machineId ++ " - " ++ show (length buttons) ++ " buttons, " ++ show (length targets) ++ " positions\n"
            smtCode = generateSMT machineId buttons targets
            content = header ++ smtCode
        writeFile filename content
        putStrLn $ "Generated " ++ filename) (zip [1..] machineData)
    
    putStrLn "Done!"