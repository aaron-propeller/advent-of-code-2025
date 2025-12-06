{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Day01.Day01 (partA, partB) where

import Data.List (foldl')

type Input = [String]
type Output = Int

data Direction = L | R deriving (Show, Read)
type Rotation = (Direction, Position)

newtype Position = Position Int deriving (Show, Eq, Num)
newtype ZeroCount = ZeroCount Int deriving (Show, Num)

data State = State 
  { position :: Position
  , zeroCount :: ZeroCount
  } deriving Show

fullRotation :: Int
fullRotation = 100

initialPosition :: Position
initialPosition = Position 50

initialState :: State
initialState = State initialPosition (ZeroCount 0)

partA :: Input -> Output
partA rotationStrings = 
  let rotations = map parseRotation rotationStrings
      State _ (ZeroCount finalCount) = foldl' processRotationA initialState rotations
  in finalCount

partB :: Input -> Output
partB rotationStrings = 
  let rotations = map parseRotation rotationStrings
      State _ (ZeroCount finalCount) = foldl' processRotationB initialState rotations
  in finalCount

processRotationA :: State -> Rotation -> State
processRotationA (State (Position current) (ZeroCount count)) (direction, Position magnitude) =
  let remainingMovement = magnitude `mod` fullRotation
      newPos@(Position new) = fst $ normalizePosition $ case direction of
        L -> Position (current - remainingMovement)
        R -> Position (current + remainingMovement)
      crossedZero = new == 0 && current /= 0
      newCount = ZeroCount $ count + if crossedZero then 1 else 0
  in State newPos newCount

processRotationB :: State -> Rotation -> State
processRotationB (State (Position current) (ZeroCount count)) (direction, Position magnitude) =
  let fullRotations = magnitude `div` fullRotation
      remainingMovement = magnitude `mod` fullRotation
      newLocation = case direction of
        L -> current - remainingMovement
        R -> current + remainingMovement
      (normalizedPos@(Position normalized), wasNormalized) = normalizePosition (Position newLocation)
      normalizedZeros = if current /= 0 && wasNormalized then 1 else 0
      directZeros = if not wasNormalized && normalized == 0 && current /= 0 then 1 else 0
      totalZeros = fullRotations + normalizedZeros + directZeros
      newCount = ZeroCount $ count + totalZeros
  in State normalizedPos newCount

parseRotation :: String -> Rotation
parseRotation rotation = 
  let direction = read $ take 1 rotation
      magnitude = Position $ read $ drop 1 rotation
  in (direction, magnitude)

normalizePosition :: Position -> (Position, Bool)
normalizePosition (Position pos)
  | pos < 0 = (Position (pos + fullRotation), True)
  | pos >= fullRotation = (Position (pos - fullRotation), True)
  | otherwise = (Position pos, False)
