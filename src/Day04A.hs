module Day04A (solve) where
import AoCUtils (parseGrid, Grid, Coord, neighbors8)
import Data.Map (toList) 
import qualified Data.Map as Map

type Input = [String]
type Output = Int

solve :: Input -> Output
solve input =
  let grid = parseGrid input
      validPaperRolls = filter (sorroundedByLessThanFour grid) (toList grid)
  in length validPaperRolls

sorroundedByLessThanFour :: Grid -> (Coord, Char) -> Bool
sorroundedByLessThanFour _ (_, '.') = False 
sorroundedByLessThanFour grid (coord, _) =
  let neighbors = neighbors8 coord
      occupiedNeighbors = filter (\c -> Map.lookup c grid == Just '@') neighbors
  in length occupiedNeighbors < 4
