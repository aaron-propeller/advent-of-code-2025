module AoCDays.Day07 (partA, partB) where

import AoCUtils.AoCGrid (Grid, Coord, parseGrid, moveCoord, Direction(East, West, South))
import Debug.Trace (traceShow)
import Data.Map (Map, toList) 
import qualified Data.Map as Map
import System.Posix.Internals (statGetType)
import AoCUtils.AoCList (count)
import qualified Data.Set as Set

type Input = [String]
type Output = Int

data State = State 
  { positions :: [Coord]
  , splitCount :: Int
  , grid :: Grid 
  } deriving Show


-- find starting position of S
-- move all positions south until we find a splitter ^
-- record the split, add the two new positions to the list, repeat until we find the edge of the grid

partA :: Input -> Output
partA input = 
  let grid = parseGrid input
      (startingPosition, _) = head . filter (\(_, c) -> c == 'S') $ toList grid
      initialState = State [startingPosition] 0 grid
      (State _ finalSplits _) = runSimulation initialState
  in traceShow startingPosition finalSplits

type EndPositionsReachable = Int
type VisitedCache = Map.Map Coord EndPositionsReachable
type ToVisit = [Coord]

partB :: Input -> Output  
partB input = 
  let grid = parseGrid input
      (startingPosition, _) = head . filter (\(_, c) -> c == 'S') $ toList grid
      endPositionsReachable = runSimulationB grid [startingPosition] Map.empty 0
  in endPositionsReachable

runSimulation :: State -> State 
runSimulation (State [] splitCount grid) = State [] splitCount grid
runSimulation (State positions splitCount grid) = 
  let (newPositions, newSplits) = foldl (\(accPositions, accSplits) pos -> 
                                            let (movedPositions, splits) = movePosition grid pos
                                            in (accPositions ++ movedPositions, accSplits + splits)
                                         ) ([], 0) positions
      newState = traceShow newPositions $ State (Set.toList $ Set.fromList newPositions) (splitCount + newSplits) grid
  in traceShow "runSimulation" runSimulation newState

-- perform a depth first search keeping track of the amount of reachable end posistions at each coord

runSimulationB :: Grid -> ToVisit -> VisitedCache -> EndPositionsReachable -> EndPositionsReachable
runSimulationB grid [] visitedCache endPositionsReachable = endPositionsReachable 
runSimulationB grid (pos:toVisit) visitedCache endPositionsReachable = 
  case Map.lookup pos visitedCache of
    Just reachable -> 
      runSimulationB grid toVisit visitedCache (endPositionsReachable + reachable)
    Nothing -> 
      let (newPositions, _) = movePosition grid pos
      in if null newPositions 
         then -- reached the end
           let newVisitedCache = Map.insert pos 1 visitedCache
           in runSimulationB grid toVisit newVisitedCache (endPositionsReachable + 1)
         else 
           let (reachableFromNew, newVisitedCache) = foldl (\(accReachable, accCache) newPos -> 
                                                              let (reachable, updatedCache) = explorePosition grid newPos accCache
                                                              in (accReachable + reachable, updatedCache)
                                                           ) (0, visitedCache) newPositions
               finalVisitedCache = Map.insert pos reachableFromNew newVisitedCache
           in runSimulationB grid toVisit finalVisitedCache (endPositionsReachable + reachableFromNew)

explorePosition :: Grid -> Coord -> VisitedCache -> (EndPositionsReachable, VisitedCache)
explorePosition grid pos visitedCache = 
  case Map.lookup pos visitedCache of
    Just reachable -> (reachable, visitedCache)
    Nothing -> 
      let (newPositions, _) = movePosition grid pos
      in if null newPositions 
         then -- reached the end
           let newVisitedCache = Map.insert pos 1 visitedCache
           in (1, newVisitedCache)
         else 
           let (reachableFromNew, newVisitedCache) = foldl (\(accReachable, accCache) newPos -> 
                                                              let (reachable, updatedCache) = explorePosition grid newPos accCache
                                                              in (accReachable + reachable, updatedCache)
                                                           ) (0, visitedCache) newPositions
               finalVisitedCache = Map.insert pos reachableFromNew newVisitedCache
           in (reachableFromNew, finalVisitedCache)

-- get next south position 
-- if ^ 
--   add east and west positions to new positions 
--   increment split count
-- else if edge of grid 
--  do not add position to new positions 
--  else 
--  add south position to new positions 
--  return (new positions, new splits)

movePosition :: Grid -> Coord -> ([Coord], Int) 
movePosition grid position = 
  let southPos = moveCoord South position
      cell = Map.lookup southPos grid
  in case cell of
    Just '^' -> 
      let eastPos = moveCoord East southPos
          westPos = moveCoord West southPos
      in ([eastPos, westPos], 1)
    Nothing -> ([], 0)
    Just _ -> ([southPos], 0)
