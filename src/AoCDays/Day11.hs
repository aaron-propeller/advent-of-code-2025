module AoCDays.Day11 (partA, partB) where
import Data.Map (Map, insert, empty, lookup)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude hiding (lookup)
import AoCUtils.AoCParsing (splitOn, splitAtSep)
import Data.Char (isSpace)
import Debug.Trace (traceShow)

type Input = [String]
type Output = Int

type Node = String

type Graph = Map Node [Node]

-- Cache for all computed paths - stores counts
type PathCache = Map (Node, Node) Int

partA :: Input -> Output
partA input = 
    let graph = parseInput input
        start = "you"
        end = "out"
        (count, _) = countPathsWithCache graph start end Map.empty
    in count

partB :: Input -> Output  
partB input = 
    let graph = parseInput input
        start = "svr"
        end = "out"
        -- Through dac then fft - reuse cache between searches
        (count1a, cache1) = countPathsWithCache graph start "dac" Map.empty
        (count1b, cache2) = countPathsWithCache graph "dac" "fft" cache1
        (count1c, cache3) = countPathsWithCache graph "fft" end cache2
        paths1 = count1a * count1b * count1c
        
        -- Through fft then dac - reuse accumulated cache
        (count2a, cache4) = countPathsWithCache graph start "fft" cache3
        (count2b, cache5) = countPathsWithCache graph "fft" "dac" cache4
        (count2c, _) = countPathsWithCache graph "dac" end cache5
        paths2 = count2a * count2b * count2c
        
    in paths1 + paths2

-- abc: xyz edf
parseInput :: Input -> Graph
parseInput = foldr parseLine empty
  where
    parseLine line acc =
      let (node, neighbours) = splitAtSep ':' line
          cleanNeighbors = words neighbours
      in insert node cleanNeighbors acc

-- Simple count-based caching - much more efficient
countPathsWithCache :: Graph -> Node -> Node -> PathCache -> (Int, PathCache)
countPathsWithCache graph start end = dfs start [start]
  where
    dfs current path memo
      | current == end = (1, memo)
      | Map.member (current, end) memo = 
          -- Use cached count from this node to end
          (Map.findWithDefault 0 (current, end) memo, memo)
      | otherwise =
          case lookup current graph of
            Nothing -> 
              let newMemo = Map.insert (current, end) 0 memo
              in (0, newMemo)
            Just neighbors ->
              let validNeighbors = filter (`notElem` path) neighbors
                  (totalCount, finalMemo) = foldl explorePath (0, memo) validNeighbors
                  updatedMemo = Map.insert (current, end) totalCount finalMemo
              in (totalCount, updatedMemo)
      where
        explorePath (accCount, accMemo) neighbor =
          let newPath = neighbor : path
              (neighborCount, newMemo) = dfs neighbor newPath accMemo
          in (accCount + neighborCount, newMemo)

