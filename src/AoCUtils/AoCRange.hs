module AoCUtils.AoCRange where

import Text.Read (readMaybe)

-- Range type alias
type Range = (Int, Int)

-- Parse range from format "X-Y"
parseRange :: String -> Maybe Range
parseRange str = 
  case break (== '-') str of
    (start, '-':end) -> do
      s <- readMaybe start
      e <- readMaybe end
      return (s, e)
    _ -> Nothing

-- Check if a value is within a range (inclusive)
inRange :: Int -> Range -> Bool
inRange x (start, end) = x >= start && x <= end

-- Check if two ranges overlap
rangeOverlap :: Range -> Range -> Bool
rangeOverlap (s1, e1) (s2, e2) = s1 <= e2 && s2 <= e1

-- Get the size of a range
rangeSize :: Range -> Int
rangeSize (start, end) = end - start + 1

-- Merge two overlapping ranges (returns Nothing if they don't overlap)
mergeRanges :: Range -> Range -> Maybe Range
mergeRanges r1@(s1, e1) r2@(s2, e2)
  | rangeOverlap r1 r2 = Just (min s1 s2, max e1 e2)
  | otherwise = Nothing