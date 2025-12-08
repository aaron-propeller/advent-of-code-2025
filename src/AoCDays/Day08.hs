module AoCDays.Day08 (partA, partB) where
import AoCUtils.AoCGrid (Coord3D, manhattenDistance3D, euclideanDistance)
import AoCUtils.AoCParsing (extractInts)
import Data.List (sortBy, minimumBy)
import Data.Ord (comparing, Down (Down))
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)

type Input = [String]
type Output = Int

type Circuit = Set Coord3D

partA :: Input -> Output
partA input = 
  let coords = map parseInput input
      coordPairsWithDistance = [(a,b, euclideanDistance a b) | a <- coords, b <- coords, a /= b]
      -- Remove any duplicate pairs (a,b) and (b,a) 
      coordsWithoutDups = Set.toList $ Set.fromList $ map (\(a,b,d) -> if a < b then (a,b,d) else (b,a,d)) coordPairsWithDistance
      sortedPairs = sortBy (comparing (\(_,_,d) -> d)) coordsWithoutDups
      circuits = map Set.singleton coords :: [Circuit]
      finalCircuits = findCircuits sortedPairs circuits 1000
      sortedFinalCircuits = sortBy (comparing (Data.Ord.Down . Set.size)) finalCircuits 
      mult = product $ take 3 $ map Set.size sortedFinalCircuits 
  in mult

partB :: Input -> Output  
partB input =
  let coords = map parseInput input
      coordPairsWithDistance = [(a,b, euclideanDistance a b) | a <- coords, b <- coords, a /= b]
      -- Remove any duplicate pairs (a,b) and (b,a) 
      coordsWithoutDups = Set.toList $ Set.fromList $ map (\(a,b,d) -> if a < b then (a,b,d) else (b,a,d)) coordPairsWithDistance
      sortedPairs = sortBy (comparing (\(_,_,d) -> d)) coordsWithoutDups
      circuits = map Set.singleton coords :: [Circuit]
      ((x1, _, _), (x2, _, _)) = findCircuitsB sortedPairs circuits
  in x1 * x2

parseInput :: String -> Coord3D
parseInput = listToCoord3D . extractInts

listToCoord3D :: [Int] -> Coord3D
listToCoord3D [x,y,z] = (x,y,z)
listToCoord3D _ = error "Invalid input for Coord3D"

findCircuits :: [(Coord3D, Coord3D, Double)] -> [Circuit] -> Int -> [Circuit]
findCircuits _ circuits 0 = circuits
findCircuits [] circuits _ = circuits
findCircuits ((a, b, _):restPairs) circuits count =
  let (circuitsWithClosestCoords, otherCircuits) = 
        foldr (\circuit (withCoords, withoutCoords) -> 
                  if Set.member a circuit || Set.member b circuit
                  then (circuit : withCoords, withoutCoords)
                  else (withCoords, circuit : withoutCoords)
              ) ([], []) circuits
      hasNewConnection = length circuitsWithClosestCoords > 1
  in  findCircuits restPairs (otherCircuits ++ [Set.unions circuitsWithClosestCoords]) (count-1)

findCircuitsB :: [(Coord3D, Coord3D, Double)] -> [Circuit] -> (Coord3D, Coord3D)
findCircuitsB ((a, b, _):restPairs) circuits =
  let (circuitsWithClosestCoords, otherCircuits) = 
        foldr (\circuit (withCoords, withoutCoords) -> 
                  if Set.member a circuit || Set.member b circuit
                  then (circuit : withCoords, withoutCoords)
                  else (withCoords, circuit : withoutCoords)
              ) ([], []) circuits
  in case otherCircuits of
      [] -> (a,b)
      _  -> findCircuitsB restPairs (otherCircuits ++ [Set.unions circuitsWithClosestCoords])

fst3 :: (a,b,c) -> a 
fst3 (x,_,_) = x 

snd3 :: (a,b,c) -> b 
snd3 (_,y,_) = y
-- Make list of all coords 
-- Find the pair with the smallest distance that isn't already in a circuit together
-- Join the two circuits 
-- Repeat 1000 times
