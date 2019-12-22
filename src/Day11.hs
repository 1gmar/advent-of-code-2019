{-# LANGUAGE RecordWildCards #-}

module Day11
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Char      (digitToInt, isDigit)
import           Data.List      (find, groupBy, maximum, minimum, sortOn, unionBy, (\\))
import           IntCodeProgram

type Position = (Int, Int)

type PanelGrid = [Panel]

data Direction
  = LEFT
  | RIGHT
  | UP
  | DOWN
  deriving (Enum)

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
    , software  :: ProgramState
    , grid      :: PanelGrid
    }

currentColor :: Robot -> Color
currentColor Robot {..} =
  case find (== panel) grid of
    Just (Panel _ color) -> color
    Nothing              -> Black

paintCurrentPanel :: Robot -> Color -> Robot
paintCurrentPanel robot@Robot {..} newColor =
  case find (== panel) grid of
    Just pan -> robot {grid = (pan {color = newColor}) : (grid \\ [pan])}
    Nothing  -> robot {grid = Panel (position panel) newColor : grid}

rotateRobot :: Robot -> Direction -> Robot
rotateRobot robot@Robot {..} newDirection =
  case (newDirection, direction) of
    (LEFT, DOWN)  -> robot {direction = RIGHT}
    (LEFT, RIGHT) -> robot {direction = UP}
    (LEFT, UP)    -> robot {direction = LEFT}
    (RIGHT, DOWN) -> robot {direction = LEFT}
    (RIGHT, LEFT) -> robot {direction = UP}
    (RIGHT, UP)   -> robot {direction = RIGHT}
    _             -> robot {direction = DOWN}

moveRobot :: Robot -> Robot
moveRobot robot@(Robot pan@(Panel (x, y) _) dir _ _) =
  case dir of
    LEFT  -> robot {panel = pan {position = (x - 1, y)}}
    RIGHT -> robot {panel = pan {position = (x + 1, y)}}
    UP    -> robot {panel = pan {position = (x, y - 1)}}
    DOWN  -> robot {panel = pan {position = (x, y + 1)}}

paintShip :: Robot -> Either String Robot
paintShip robot@Robot {..} =
  case robot of
    Robot _ _ (ProgramState _ _ _ _ _ _ True) _ -> Right robot
    Robot _ _ state _ -> do
      let color = fromEnum $ currentColor robot
      stateWithColor <- runIntCodeProgram state {input = [color, color]}
      stateWithDirection <- runIntCodeProgram stateWithColor
      let brushRobot = paintCurrentPanel robot $ toEnum (result stateWithColor)
      let rotatedRobot = rotateRobot brushRobot $ toEnum (result stateWithDirection)
      paintShip $ (moveRobot rotatedRobot) {software = stateWithDirection}

runPaintingRobot :: Color -> [String] -> Either String Robot
runPaintingRobot startColor prog = paintShip $ Robot pan UP soft [pan]
  where
    soft = ProgramState 0 [] prog 0 0 True False
    pan = Panel (0, 0) startColor

countPaintedPanels :: Robot -> Int
countPaintedPanels Robot {..} = length grid

showPanel :: Panel -> Char
showPanel Panel {..} =
  case color of
    Black -> '⬜'
    White -> '⬛'

fillGridLine :: (Int, Int) -> PanelGrid -> PanelGrid
fillGridLine (lower, upper) ~line@(Panel (_, y) _:_) = sortOn (fst . position) fullLine
  where
    fullLine = unionBy xCoordinate line fillerLine
    fillerLine = map (`Panel` Black) $ [lower .. upper] `zip` repeat y
    xCoordinate (Panel (x1, _) _) (Panel (x2, _) _) = x1 == x2

showRegistrationNumber :: Robot -> String
showRegistrationNumber Robot {..} = unlines $ foldr showPanels [] fullGridLines
  where
    xs = map (fst . position) grid
    [minX, maxX] = map (\f -> f xs) [minimum, maximum]
    gridLines = groupBy yCoordinate $ sortOn (snd . position) grid
    fullGridLines = fillGridLine (minX, maxX) <$> gridLines
    yCoordinate (Panel (_, y1) _) (Panel (_, y2) _) = y1 == y2
    showPanels gridRow rows = map showPanel gridRow : rows

writeResult :: Either String String -> IO ()
writeResult (Left err)     = putStrLn err
writeResult (Right panels) = writeFile "./out/output-day11.txt" panels

showCount :: Either String Int -> String
showCount (Left err)  = "Error: " ++ err
showCount (Right res) = show res

inputFile :: String
inputFile = "./resources/input-day11.txt"

solutionPart1 :: IO ()
solutionPart1 = readInputData inputFile >>= putStrLn . showCount . fmap countPaintedPanels . runPaintingRobot Black

solutionPart2 :: IO ()
solutionPart2 = readInputData inputFile >>= writeResult . fmap showRegistrationNumber . runPaintingRobot White
