{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Day01A (solve) where

import Data.List (foldl')

-- Puzzle solution

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

solve :: Input -> Output
solve rotationStrings = 
  let rotations = map parseRotation rotationStrings
      State _ (ZeroCount finalCount) = foldl' processRotation initialState rotations
  in finalCount

processRotation :: State -> Rotation -> State
processRotation (State (Position current) (ZeroCount count)) (direction, Position magnitude) =
  let remainingMovement = magnitude `mod` fullRotation
      newPos@(Position new) = normalizePosition $ case direction of
        L -> Position (current - remainingMovement)
        R -> Position (current + remainingMovement)
      crossedZero = new == 0 && current /= 0
      newCount = ZeroCount $ count + if crossedZero then 1 else 0
  in State newPos newCount

parseRotation :: String -> Rotation
parseRotation rotation = 
  let direction = read $ take 1 rotation
      magnitude = Position $ read $ drop 1 rotation
  in (direction, magnitude)

normalizePosition :: Position -> Position
normalizePosition (Position pos)
  | pos < 0 = Position (pos + fullRotation)
  | pos >= fullRotation = Position (pos - fullRotation)
  | otherwise = Position pos