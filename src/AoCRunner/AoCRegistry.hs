module AoCRunner.AoCRegistry where

import qualified Data.Map as Map
import qualified AoCDays.Day01 as Day01
import qualified AoCDays.Day02 as Day02
import qualified AoCDays.Day03 as Day03
import qualified AoCDays.Day04 as Day04
import qualified AoCDays.Day05 as Day05
import qualified AoCDays.Day06 as Day06
import qualified AoCDays.Day07 as Day07
import qualified AoCDays.Day08 as Day08
import qualified AoCDays.Day09 as Day09
import qualified AoCDays.Day10 as Day10
import qualified AoCDays.Day11 as Day11
import qualified AoCDays.Day12 as Day12

type DayRunner = [String] -> (Int, Int)
type DayNumber = String

-- Registry of all available day runners
dayRunners :: Map.Map DayNumber DayRunner
dayRunners = Map.fromList
  [ ("01", \input -> (Day01.partA input, Day01.partB input))
  , ("02", \input -> (Day02.partA input, Day02.partB input))
  , ("03", \input -> (Day03.partA input, Day03.partB input))
  , ("04", \input -> (Day04.partA input, Day04.partB input))
  , ("05", \input -> (Day05.partA input, Day05.partB input))
  , ("06", \input -> (Day06.partA input, Day06.partB input))
  , ("07", \input -> (Day07.partA input, Day07.partB input))
  , ("08", \input -> (Day08.partA input, Day08.partB input))
  , ("09", \input -> (Day09.partA input, Day09.partB input))
  , ("10", \input -> (Day10.partA input, Day10.partB input))
  , ("11", \input -> (Day11.partA input, Day11.partB input))
  , ("12", \input -> (Day12.partA input, Day12.partB input))
  ]

-- Get list of available days
availableDays :: [DayNumber]
availableDays = Map.keys dayRunners

-- Look up a day runner
getDayRunner :: DayNumber -> Maybe DayRunner
getDayRunner = flip Map.lookup dayRunners

-- Register a new day (for future extensibility)
registerDay :: DayNumber -> DayRunner -> Map.Map DayNumber DayRunner -> Map.Map DayNumber DayRunner
registerDay = Map.insert