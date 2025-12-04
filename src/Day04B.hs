module Day04B (solve) where
import AoCUtils (parseGrid, Grid, Coord, neighbors8)
import Data.Map (toList) 
import qualified Data.Map as Map

type Input = [String]
type Output = Int

solve :: Input -> Output
solve input =
  let grid = parseGrid input
      totalRemovals = countValidPaperRolls grid 
  in totalRemovals

sorroundedByLessThanFour :: Grid -> (Coord, Char) -> Bool
sorroundedByLessThanFour _ (_, '.') = False 
sorroundedByLessThanFour grid (coord, _) =
  let neighbors = neighbors8 coord
      occupiedNeighbors = filter (\c -> Map.lookup c grid == Just '@') neighbors
  in length occupiedNeighbors < 4

countValidPaperRolls :: Grid -> Int 
countValidPaperRolls grid =
  let validPaperRolls = filter (sorroundedByLessThanFour grid) (toList grid)
  in if null validPaperRolls
     then 0
     else let newGrid = foldr (\(coord, _) g -> Map.insert coord '.' g) grid validPaperRolls
          in length validPaperRolls + countValidPaperRolls newGrid
