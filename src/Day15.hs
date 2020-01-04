{-# LANGUAGE RecordWildCards #-}

module Day15
  ( solutionPart1
  , solutionPart2
  ) where

import           IntCodeProgram

type PriorityQueue = [Node]

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

data Node =
  Node
    { direction   :: Direction
    , moves       :: Int
    , state       :: ProgramState
    , destination :: Destination
    }

(<:) :: Node -> PriorityQueue -> PriorityQueue
(<:) node [] = [node]
(<:) node queue@(x:xs)
  | moves node > moves x = x : node <: xs
  | otherwise = node : queue

(<++) :: PriorityQueue -> PriorityQueue -> PriorityQueue
(<++) [] ys     = ys
(<++) (x:xs) ys = x <: (xs <++ ys)

neighbors :: Node -> [Node]
neighbors node@Node {..} = map neighbor $ filter (/= backwards direction) (enumFrom North)
  where
    neighbor dir = node {direction = dir, moves = moves + 1}
    backwards dir =
      case dir of
        Center -> Center
        North  -> South
        West   -> East
        _      -> (toEnum . subtract 1 . fromEnum) dir

moveDroid :: Node -> Either String Node
moveDroid node@Node {..} = do
  movedState <- runIntCodeProgram state {input = [fromEnum direction]}
  let dest = toEnum $ result movedState
  Right node {state = movedState, destination = dest}

moveDroids :: [Node] -> Either String [Node]
moveDroids = fmap dodgeWalls . mapM moveDroid
  where
    dodgeWalls = filter ((/= Wall) . destination)

searchMinPath :: PriorityQueue -> Either String Node
searchMinPath [] = Left "Illegal priority queue state!"
searchMinPath (node:queue)
  | destination node == OxygenSystem = Right node
  | otherwise = moveDroids (neighbors node) >>= searchMinPath . (<++ queue)

spreadOxygen :: Int -> [Node] -> Either String Int
spreadOxygen minutes edges
  | null edges = Right $ subtract 1 minutes
  | otherwise = moveDroids (concatMap neighbors edges) >>= spreadOxygen (minutes + 1)

findOxygenMinutes :: [Node] -> Either String Int
findOxygenMinutes startNode = do
  oxygenNode <- searchMinPath startNode
  spreadOxygen 0 [oxygenNode {direction = Center}]

startingQueue :: [String] -> [Node]
startingQueue prog = [Node Center 0 (programState prog) Empty]

printResult :: Either String Int -> IO ()
printResult (Left err)  = putStrLn $ "Error: " ++ err
printResult (Right res) = print res

inputFile :: String
inputFile = "./resources/input-day15.txt"

solutionPart1 :: IO ()
solutionPart1 = readInputData inputFile >>= printResult . fmap moves . searchMinPath . startingQueue

solutionPart2 :: IO ()
solutionPart2 = readInputData inputFile >>= printResult . findOxygenMinutes . startingQueue
