{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Day11
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.List      (find, groupBy, maximum, minimum, sortOn, unionBy, (\\))
import           IntCodeProgram hiding (showResult)

type Position = (Int, Int)

type PanelGrid = [Panel]

class (Eq a, Enum a, Bounded a) =>
      CyclicEnum a
  where
  cPred :: a -> a
  cPred value
    | value == minBound = maxBound
    | otherwise = pred value
  cSucc :: a -> a
  cSucc value
    | value == maxBound = minBound
    | otherwise = succ value

data Direction
  = UP
  | RIGHT
  | DOWN
  | LEFT
  deriving (Eq, Enum, Bounded, CyclicEnum)

data Color
  = Black
  | White
  deriving (Enum, Eq)

data Panel =
  Panel
    { position :: Position
    , color    :: Color
    }

instance Eq Panel where
  (Panel pos1 _) == (Panel pos2 _) = pos1 == pos2

data Robot =
  Robot
    { panel     :: Panel
    , direction :: Direction
    , state     :: ProgramState
    , grid      :: PanelGrid
    }

currentColor :: Robot -> Color
currentColor Robot {..} =
  case find (== panel) grid of
    Just (Panel _ color) -> color
    Nothing              -> Black

paintCurrentPanel :: Robot -> Int -> Either String Robot
paintCurrentPanel robot@Robot {..} colorCode
  | colorCode `elem` [0, 1] = Right paintedPanel
  | otherwise = Left $ "Unknown color code: " ++ show colorCode
  where
    newColor = toEnum colorCode
    newPanel = robot {grid = Panel (position panel) newColor : grid}
    updatePanel pan = robot {grid = (pan {color = newColor}) : (grid \\ [pan])}
    paintedPanel = maybe newPanel updatePanel $ find (== panel) grid

rotateRobot :: Robot -> Int -> Either String Robot
rotateRobot robot@Robot {..} directionCode =
  case directionCode of
    0 -> Right robot {direction = cPred direction}
    1 -> Right robot {direction = cSucc direction}
    _ -> Left $ "Unknown direction code: " ++ show directionCode

moveRobot :: Robot -> Robot
moveRobot robot@(Robot pan@(Panel (x, y) _) dir _ _) =
  case dir of
    LEFT  -> robot {panel = pan {position = (x - 1, y)}}
    RIGHT -> robot {panel = pan {position = (x + 1, y)}}
    UP    -> robot {panel = pan {position = (x, y - 1)}}
    DOWN  -> robot {panel = pan {position = (x, y + 1)}}

paintShip :: Robot -> Either String Robot
paintShip robot@Robot {..}
  | halted state = Right robot
  | otherwise = do
    let color = fromEnum $ currentColor robot
    interruptedState <- runIntCodeProgram state {input = [color]}
    (nextColor, nextDirection) <- extractOutput $ output interruptedState
    brushRobot <- paintCurrentPanel robot nextColor
    rotatedRobot <- rotateRobot brushRobot nextDirection
    paintShip $ (moveRobot rotatedRobot) {state = interruptedState {output = []}}
  where
    extractOutput out =
      case out of
        [nextColor, nextDirection] -> Right (nextColor, nextDirection)
        _                          -> Left $ "Incompatible output data: " ++ show out

runPaintingRobot :: Color -> [Int] -> Either String Robot
runPaintingRobot startColor prog = paintShip $ Robot pan UP soft [pan]
  where
    soft = programState prog
    pan = Panel (0, 0) startColor

countPaintedPanels :: Robot -> Int
countPaintedPanels Robot {..} = length grid

showPanel :: Panel -> Char
showPanel Panel {..} =
  case color of
    Black -> '⬜'
    White -> '⬛'

xPos :: Panel -> Int
xPos = fst . position

yPos :: Panel -> Int
yPos = snd . position

compareOn :: (Panel -> Int) -> Panel -> Panel -> Bool
compareOn fPos panel1 panel2 = fPos panel1 == fPos panel2

fillGridLine :: (Int, Int) -> PanelGrid -> PanelGrid
fillGridLine (lower, upper) ~line@(Panel (_, y) _:_) = sortOn xPos fullLine
  where
    fullLine = unionBy (compareOn xPos) line fillerLine
    fillerLine = map (`Panel` Black) $ [lower .. upper] `zip` repeat y

showRegistrationNumber :: Robot -> String
showRegistrationNumber Robot {..} = unlines $ foldr showPanels [] fullGridLines
  where
    xs = map xPos grid
    [minX, maxX] = map (\f -> f xs) [minimum, maximum]
    gridLines = groupBy (compareOn yPos) $ sortOn yPos grid
    fullGridLines = fillGridLine (minX, maxX) <$> gridLines
    showPanels gridRow rows = map showPanel gridRow : rows

writeResult :: Either String String -> IO ()
writeResult (Left err)     = putStrLn err
writeResult (Right panels) = writeFile "./resources/output/day11.txt" panels

showResult :: Either String Int -> String
showResult (Left err)  = "Error: " ++ err
showResult (Right res) = show res

inputFile :: String
inputFile = "./resources/input/day11.txt"

solutionPart1 :: IO ()
solutionPart1 = readInputData inputFile >>= putStrLn . showResult . fmap countPaintedPanels . runPaintingRobot Black

solutionPart2 :: IO ()
solutionPart2 = readInputData inputFile >>= writeResult . fmap showRegistrationNumber . runPaintingRobot White
