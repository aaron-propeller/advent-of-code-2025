module AoCDays.Day08 (partA, partB) where
import AoCUtils.AoCGrid (Coord3D, manhattenDistance3D, euclideanDistance)
import AoCUtils.AoCParsing (extractInts)
import AoCUtils.AoCList (uniquePairs)
import AoCUtils.AoCTuple (fst3, snd3, thd3)
import Data.List (sortBy, minimumBy)
import Data.Ord (comparing, Down (Down))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Heap as Heap
import Debug.Trace (traceShow)

type Input = [String]
type Output = Int

type Circuit = Set Coord3D

partA :: Input -> Output
partA input = 
    let coords = map parseInput input
        pairHeap = Heap.fromList [Heap.Entry (euclideanDistance a b) (a,b) | (a,b) <- uniquePairs coords]
        circuits = map Set.singleton coords :: [Circuit]
        finalCircuits = findCircuitsHeap pairHeap circuits 1000
        sortedFinalCircuits = sortBy (comparing (Data.Ord.Down . Set.size)) finalCircuits 
        mult = product $ take 3 $ map Set.size sortedFinalCircuits 
    in mult

partB :: Input -> Output  
partB input =
  let coords = map parseInput input
      pairHeap = Heap.fromList [Heap.Entry (euclideanDistance a b) (a,b) | (a,b) <- uniquePairs coords]
      circuits = map Set.singleton coords :: [Circuit]
      (a, b) = findCircuitsBHeap pairHeap circuits
  in fst3 a * fst3 b

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

findCircuitsHeap :: Heap.Heap (Heap.Entry Double (Coord3D, Coord3D)) -> [Circuit] -> Int -> [Circuit]
findCircuitsHeap heap circuits 0 = circuits
findCircuitsHeap heap circuits count =
  case Heap.uncons heap of
    Nothing -> circuits
    Just (Heap.Entry _ (a, b), restHeap) ->
      let (circuitsWithClosestCoords, otherCircuits) = 
            foldr (\circuit (withCoords, withoutCoords) -> 
                      if Set.member a circuit || Set.member b circuit
                      then (circuit : withCoords, withoutCoords)
                      else (withCoords, circuit : withoutCoords)
                  ) ([], []) circuits
      in findCircuitsHeap restHeap (otherCircuits ++ [Set.unions circuitsWithClosestCoords]) (count-1)

findCircuitsBHeap :: Heap.Heap (Heap.Entry Double (Coord3D, Coord3D)) -> [Circuit] -> (Coord3D, Coord3D)
findCircuitsBHeap heap circuits =
  case Heap.uncons heap of
    Nothing -> error "Empty heap"
    Just (Heap.Entry _ (a, b), restHeap) ->
      let (circuitsWithClosestCoords, otherCircuits) = 
            foldr (\circuit (withCoords, withoutCoords) -> 
                      if Set.member a circuit || Set.member b circuit
                      then (circuit : withCoords, withoutCoords)
                      else (withCoords, circuit : withoutCoords)
                  ) ([], []) circuits
      in case otherCircuits of
          [] -> (a,b)
          _  -> findCircuitsBHeap restHeap (otherCircuits ++ [Set.unions circuitsWithClosestCoords])

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

