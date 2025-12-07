module AoCDays.Day07 (partA, partB) where

import AoCUtils.AoCGrid (Grid, Coord, parseGrid, moveCoord, Direction(East, West, South), locationsOf, groupByRow)
import Debug.Trace (traceShow)
import Data.Map (Map, toList) 
import qualified Data.Map as Map
import System.Posix.Internals (statGetType)
import AoCUtils.AoCList (count, freqMap)
import qualified Data.Set as Set
import Data.List (groupBy, sortBy, nub, intersect, (\\))
import Data.Function (on)
import Control.Arrow ((&&&))

type Input = [String]
type Output = Int

-- iterate over each splitter location row 
-- Build a new map by checking to see if each current location touches a splitter 
-- If it does, split the location and add one to the splitCount

partA :: Input -> Output
partA input = 
  let (currentLocations, groupedByRowSplitters) = parseInput input
      finalState = foldl processRow (currentLocations, 0) groupedByRowSplitters
  in snd finalState

partB :: Input -> Output  
partB input =
  let (currentLocations, groupedByRowSplitters) = parseInput input
      finalState = foldl processRow (currentLocations, 0) groupedByRowSplitters
  in sum . Map.elems $ fst finalState


parseInput :: [String] -> (Map Int Int, [[Int]])
parseInput = (startPositions &&& splitterRows) . parseGrid
  where
    startPositions = freqMap . map fst . locationsOf 'S'
    splitterRows = map (map fst) . groupByRow . locationsOf '^'

type State = (Map Int Int, Int) -- (current location map, split count)

processRow :: State -> [Int] -> State
processRow (currentMap, splitCount) splitters =
    let (hits, misses) = Map.partitionWithKey (const . (`elem` splitters)) currentMap
        splitBeam pos count = [(pos-1, count), (pos+1, count)]
        newPositions = Map.fromListWith (+) $ concatMap (uncurry splitBeam) (Map.toList hits)
    in (Map.unionWith (+) misses newPositions, splitCount + Map.size hits)
