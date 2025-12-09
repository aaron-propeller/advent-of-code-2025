module AoCDays.Day09 (partA, partB) where
import AoCUtils.AoCParsing (extractInts)
import Data.Ratio
import AoCUtils.AoCList (uniquePairs, chunks, windows)
import Data.Ord (Down(Down))
import qualified Data.Heap as Heap
import AoCUtils.AoCGrid (Coord)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Data.List (sort, sortOn)
import Control.Parallel.Strategies (parMap, rpar, using, parList)

type Input = [String]
type Output = Int

partA :: Input -> Output
partA input = 
  let squares = map (tuplify2 . extractInts) input
      pairs = uniquePairs squares 
      pairHeap = Heap.fromList [Heap.Entry (Down (calculateArea a b)) (a,b) | (a,b) <- pairs]
      
  in case Heap.uncons pairHeap of
    Nothing -> 0-- empty
    Just (Heap.Entry (Down area) (a, b), restHeap) -> area

partB :: Input -> Output  
partB input = 
  let polygon = map (extractInts) input 
      pairs = [ (a, b) 
              | a <- polygon, 
                b <- polygon
            ]
      area = [ calculateArea (tuplify2 x)  (tuplify2 y)
          | (x, y) <- pairs,
          not (intersects (x) (y) (polygon))
        ]

  in maximum area


type Point = [Int]

coordToPoint :: Coord -> Point
coordToPoint (x, y) = [fromIntegral x, fromIntegral y]

intersects :: Point -> Point -> [Point] -> Bool
intersects [x, y] [x', y'] = not . all away . pairs
  where
    pairs (p : ps) = zip (p : ps) (ps ++ [p])
    away ([lx, ly], [lx', ly']) =
      (max lx lx' <= min x x')
        || (min lx lx' >= max x x')
        || (max ly ly' <= min y y')
        || (min ly ly' >= max y y')

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _      = error "List must contain exactly 2 elements"

calculateArea :: Coord -> Coord -> Int 
calculateArea (x1, y1) (x2, y2) = 
  let width =  (abs (x2 - x1) + 1)
      height =  (abs (y2 - y1) + 1)
  in width * height

generateStraightLineCoords :: Coord -> Coord -> [Coord]
generateStraightLineCoords (x1, y1) (x2, y2)
  | x1 == x2 = [(x1, y) | y <- range y1 y2]
  | y1 == y2 = [(x, y1) | x <- range x1 x2]
  | otherwise = error "Coordinates are not aligned straight"
  where
    range a b
      | a <= b    = [a..b]
      | otherwise = [b..a]

generateSquareCorners :: Coord -> Coord -> [Coord]
generateSquareCorners (x1, y1) (x2, y2) = 
  [(min x1 x2, min y1 y2), (min x1 x2, max y1 y2), (max x1 x2, min y1 y2), (max x1 x2, max y1 y2)]

-- Generate the boundary (perimeter) coordinates of a square defined by two corners
generateSquareBoundary :: Coord -> Coord -> [Coord]
generateSquareBoundary (x1, y1) (x2, y2) = 
  let minX = min x1 x2
      maxX = max x1 x2  
      minY = min y1 y2
      maxY = max y1 y2
      topSide = [(x, maxY) | x <- [minX..maxX]]
      bottomSide = [(x, minY) | x <- [minX..maxX]]
      leftSide = [(minX, y) | y <- [minY+1..maxY-1]]  -- Avoid corner duplicates
      rightSide = [(maxX, y) | y <- [minY+1..maxY-1]] -- Avoid corner duplicates
  in topSide ++ bottomSide ++ leftSide ++ rightSide

-- Remove duplicate coordinates
removeDuplicates :: [Coord] -> [Coord]
removeDuplicates = Set.toList . Set.fromList




-- Line segment intersection and point-in-polygon algorithms

