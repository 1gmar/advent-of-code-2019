{-# LANGUAGE DeriveAnyClass #-}

module Day11
  ( solutionPart1,
    solutionPart2,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (bimap)
import Data.List (find, groupBy, sortOn, unionBy, (\\))
import Util.CyclicEnumClass
import Util.IntCodeProgram

type Position = (Int, Int)

type PanelGrid = [Panel]

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

data Panel = Panel
  { position :: Position,
    color :: Color
  }

instance Eq Panel where
  (Panel pos1 _) == (Panel pos2 _) = pos1 == pos2

data Robot = Robot
  { panel :: Panel,
    direction :: Direction,
    state :: ProgramState,
    grid :: PanelGrid
  }

currentColor :: Robot -> Color
currentColor Robot {..} =
  case find (== panel) grid of
    Just (Panel _ color) -> color
    Nothing -> Black

paintCurrentPanel :: Robot -> Int -> Either String Robot
paintCurrentPanel robot@Robot {..} colorCode
  | colorCode `elem` [0, 1] = return paintedPanel
  | otherwise = throwError $ "Unknown color code: " ++ show colorCode
  where
    newColor = toEnum colorCode
    newPanel = robot {grid = Panel (position panel) newColor : grid}
    updatePanel pan = robot {grid = (pan {color = newColor}) : (grid \\ [pan])}
    paintedPanel = maybe newPanel updatePanel $ find (== panel) grid

rotateRobot :: Robot -> Int -> Either String Robot
rotateRobot robot@Robot {..} directionCode =
  case directionCode of
    0 -> return robot {direction = cPred direction}
    1 -> return robot {direction = cSucc direction}
    _ -> throwError $ "Unknown direction code: " ++ show directionCode

moveRobot :: Robot -> Robot
moveRobot robot@(Robot pan@(Panel (x, y) _) dir _ _) =
  case dir of
    LEFT -> robot {panel = pan {position = (x - 1, y)}}
    RIGHT -> robot {panel = pan {position = (x + 1, y)}}
    UP -> robot {panel = pan {position = (x, y - 1)}}
    DOWN -> robot {panel = pan {position = (x, y + 1)}}

paintShip :: Robot -> Either String Robot
paintShip robot@Robot {..}
  | terminated state = return robot
  | otherwise = do
    let color = fromEnum $ currentColor robot
    interruptedState <- runIntCodeProgram state {input = [color]}
    (nextColor, nextDirection) <- extractOutput $ outputList interruptedState
    brushRobot <- paintCurrentPanel robot nextColor
    rotatedRobot <- rotateRobot brushRobot nextDirection
    paintShip $ (moveRobot rotatedRobot) {state = programWithOutput interruptedState []}
  where
    extractOutput out =
      case out of
        [nextColor, nextDirection] -> return (nextColor, nextDirection)
        _ -> Left $ "Incompatible output data: " ++ show out

runPaintingRobot :: Color -> [Int] -> Either String Robot
runPaintingRobot startColor prog = paintShip $ Robot pan UP soft [pan]
  where
    soft = newProgram prog
    pan = Panel (0, 0) startColor

countPaintedPanels :: Robot -> Int
countPaintedPanels Robot {..} = length grid

showPanel :: Panel -> Char
showPanel Panel {..} =
  case color of
    Black -> '\x2B1C'
    White -> '\x2B1B'

xPos :: Panel -> Int
xPos = fst . position

yPos :: Panel -> Int
yPos = snd . position

compareOn :: (Panel -> Int) -> Panel -> Panel -> Bool
compareOn fPos panel1 panel2 = fPos panel1 == fPos panel2

fillGridLine :: (Int, Int) -> PanelGrid -> PanelGrid
fillGridLine _ [] = []
fillGridLine (lower, upper) line@(Panel (_, y) _ : _) = sortOn xPos fullLine
  where
    fullLine = unionBy (compareOn xPos) line fillerLine
    fillerLine = map (\x -> curry (`Panel` Black) x y) [lower .. upper]

showRegistrationNumber :: Robot -> String
showRegistrationNumber Robot {..} = unlines $ foldr showPanels [] fullGridLines
  where
    xs = map xPos grid
    (minX, maxX) = bimap (\f -> f xs) (\f -> f xs) (minimum, maximum)
    gridLines = groupBy (compareOn yPos) $ sortOn yPos grid
    fullGridLines = fillGridLine (minX, maxX) <$> gridLines
    showPanels gridRow rows = map showPanel gridRow : rows

solutionPart1 :: String -> Either String Int
solutionPart1 = fmap countPaintedPanels . runPaintingRobot Black . parseIntCode

solutionPart2 :: String -> Either String String
solutionPart2 = fmap showRegistrationNumber . runPaintingRobot White . parseIntCode
