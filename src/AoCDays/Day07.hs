module AoCDays.Day07 (partA, partB) where

import AoCUtils.AoCGrid (Grid, Coord, parseGrid, moveCoord, Direction(East, West, South))
import Debug.Trace (traceShow)
import Data.Map (Map, toList) 
import qualified Data.Map as Map
import System.Posix.Internals (statGetType)
import AoCUtils.AoCList (count, freqMap)
import qualified Data.Set as Set
import Data.List (groupBy, sortBy, nub, intersect, (\\))
import Data.Function (on)

type Input = [String]
type Output = Int

partA :: Input -> Output
partA input = 
  let grid = parseGrid input 
      currentLocations = freqMap $ map (fst . fst) . filter (\(_, c) -> c == 'S') $ toList grid
      splitterLocations = map fst . filter (\(_, c) -> c == '^') $ toList grid
      groupedByRowSplitters  = map (map fst) . groupBy (on (==) snd) $ sortBy (compare `on` snd) splitterLocations
      finalState = fireBeam (currentLocations, 0) groupedByRowSplitters
  in snd finalState

partB :: Input -> Output  
partB input =
  let grid = parseGrid input 
      currentLocations = freqMap $ map (fst . fst) . filter (\(_, c) -> c == 'S') $ toList grid
      splitterLocations = map fst . filter (\(_, c) -> c == '^') $ toList grid
      groupedByRowSplitters  = map (map fst) . groupBy (on (==) snd) $ sortBy (compare `on` snd) splitterLocations
      finalState = fireBeam (currentLocations, 0) groupedByRowSplitters
  in sum . Map.elems $ fst finalState


type State = (Map Int Int, Int) -- (current location map, split count)

-- iterate over each splitter location row 
-- Build a new map by checking to see if each current location touches a splitter 
-- If it does, split the location and add one to the splitCount

fireBeam :: State -> [[Int]] -> State 
fireBeam state [] = state
fireBeam (currentMap, splitCount) (currentSplitterRow:rest) =
  let (newMap, newSplits) = processRow currentMap currentSplitterRow 
  in fireBeam (newMap, splitCount + newSplits) rest

processRow :: Map Int Int -> [Int] -> (Map Int Int, Int)
processRow currentMap splitterRow = 
  let positionsThatHitSplitter = Map.keys currentMap `intersect` splitterRow 
      positionsThatMissSplitter = Map.keys currentMap \\ splitterRow 
      splitCount = length positionsThatHitSplitter
      newPositions = concatMap (\pos -> [(pos - 1, Map.lookup pos currentMap), (pos + 1, Map.lookup pos currentMap)]) positionsThatHitSplitter
      mapWithoutPositionsThatHitSplitter = Map.filterWithKey (\k _ -> k `elem` positionsThatMissSplitter) currentMap 
      newPositionsAsMap = Map.fromListWith (+) [(newPos, count) | (newPos, Just count) <- newPositions]
      combinedMaps = Map.unionWith (+) mapWithoutPositionsThatHitSplitter newPositionsAsMap
  in (combinedMaps, splitCount)
