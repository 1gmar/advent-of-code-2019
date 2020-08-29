{-# LANGUAGE RecordWildCards #-}

module Day15
  ( solutionPart1
  , solutionPart2
  ) where

import           Util.IntCodeProgram

type PriorityQueue = [Droid]

data Direction
  = Center
  | North
  | South
  | West
  | East
  deriving (Eq, Enum, Bounded)

data Destination
  = Wall
  | Empty
  | OxygenSystem
  deriving (Eq, Enum)

data Droid
  = Droid
      { direction   :: Direction
      , moves       :: Int
      , state       :: Program
      , destination :: Destination
      }

(<:) :: Droid -> PriorityQueue -> PriorityQueue
(<:) droid [] = [droid]
(<:) droid queue@(x:xs)
  | moves droid > moves x = x : droid <: xs
  | otherwise = droid : queue

(<++) :: PriorityQueue -> PriorityQueue -> PriorityQueue
(<++) [] ys     = ys
(<++) (x:xs) ys = x <: (xs <++ ys)

neighbors :: Droid -> [Droid]
neighbors droid@Droid {..} = map neighbor $ filter (/= backwards direction) (enumFrom North)
  where
    neighbor dir = droid {direction = dir, moves = moves + 1}
    backwards dir =
      case dir of
        Center -> Center
        North  -> South
        West   -> East
        _      -> (toEnum . subtract 1 . fromEnum) dir

moveDroid :: Droid -> Either String Droid
moveDroid droid@Droid {..} = do
  movedState <- runIntCodeProgram state {input = [fromEnum direction]}
  let dest = toEnum $ result movedState
  Right droid {state = movedState, destination = dest}

moveDroids :: [Droid] -> Either String [Droid]
moveDroids = fmap dodgeWalls . mapM moveDroid
  where
    dodgeWalls = filter ((/= Wall) . destination)

searchMinPath :: PriorityQueue -> Either String Droid
searchMinPath [] = Left "Illegal priority queue state!"
searchMinPath (droid:queue)
  | destination droid == OxygenSystem = Right droid
  | otherwise = moveDroids (neighbors droid) >>= searchMinPath . (<++ queue)

spreadOxygen :: Int -> [Droid] -> Either String Int
spreadOxygen minutes edges
  | null edges = Right $ minutes - 1
  | otherwise = moveDroids (concatMap neighbors edges) >>= spreadOxygen (minutes + 1)

findOxygenMinutes :: [Droid] -> Either String Int
findOxygenMinutes queue = do
  atOxygenDroid <- searchMinPath queue
  spreadOxygen 0 [atOxygenDroid {direction = Center}]

startingQueue :: [Int] -> [Droid]
startingQueue prog = [Droid Center 0 (newProgram prog) Empty]

solutionPart1 :: String -> Either String Int
solutionPart1 = fmap moves . searchMinPath . startingQueue . parseIntCode

solutionPart2 :: String -> Either String Int
solutionPart2 = findOxygenMinutes . startingQueue . parseIntCode
