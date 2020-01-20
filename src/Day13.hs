{-# LANGUAGE RecordWildCards #-}

module Day13
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.List      (find, (\\))
import           IntCodeProgram

type Grid = [Cell]

type GameResult = Either String Game

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Eq)

data Cell =
  Cell
    { position :: (Int, Int)
    , tile     :: Tile
    }

instance Eq Cell where
  Cell pos1 _ == Cell pos2 _ = pos1 == pos2

data Game =
  Game
    { state :: ProgramState
    , score :: Int
    , grid  :: Grid
    }

toTile :: Int -> Either String Tile
toTile tileCode =
  case tileCode of
    0 -> Right Empty
    1 -> Right Wall
    2 -> Right Block
    3 -> Right Paddle
    4 -> Right Ball
    _ -> Left $ "Unknown tile code: " ++ show tileCode

countBlockTiles :: Game -> Int
countBlockTiles = length . filter ((== Block) . tile) . grid

replaceCell :: Cell -> Grid -> Grid
replaceCell newCell gameGrid =
  case find (== newCell) gameGrid of
    Just cell -> newCell : (gameGrid \\ [cell])
    Nothing   -> newCell : gameGrid

collectGridCells :: Game -> [Int] -> GameResult
collectGridCells game@Game {..} outputData =
  case outputData of
    []            -> Right game
    x:y:value:out -> updateGame (x, y) value out
    _             -> Left $ "Incompatible output data: " ++ show outputData
  where
    addCell out point tileValue = collectGridCells game {grid = replaceCell (Cell point tileValue) grid} out
    updateGame point value out
      | point == (-1, 0) = collectGridCells game {score = value} out
      | otherwise = toTile value >>= addCell out point

buildGameGrid :: Game -> GameResult
buildGameGrid game = do
  nextState <- runIntCodeProgram $ state game
  collectGridCells game {state = nextState} (output nextState)

startGame :: [Int] -> Game
startGame prog = Game (programState prog) 0 []

choosePaddleCmd :: Game -> Either String Int
choosePaddleCmd Game {..} =
  case (,) <$> xPosFor Paddle <*> xPosFor Ball of
    Just xTuple -> Right $ compareX xTuple
    Nothing     -> Left "Illegal grid state!"
  where
    xPosFor tileVal = fst . position <$> find ((== tileVal) . tile) grid
    compareX (xPaddle, xBall)
      | xPaddle < xBall = 1
      | xPaddle > xBall = -1
      | otherwise = 0

playGame :: Game -> GameResult
playGame game@(Game currentState _ _)
  | halted currentState = Right game
  | otherwise = do
    nextGame <- buildGameGrid game
    paddleCmd <- choosePaddleCmd nextGame
    let nextState = state nextGame
    playGame (nextGame {state = nextState {input = [paddleCmd], output = []}})

startFreeGame :: [Int] -> Game
startFreeGame prog = Game (programState (2 : drop 1 prog)) 0 []

printResult :: Either String Int -> IO ()
printResult (Left err)  = putStrLn $ "Error: " ++ err
printResult (Right res) = print res

inputFile :: String
inputFile = "./resources/input/day13.txt"

solutionPart1 :: IO ()
solutionPart1 = readInputData inputFile >>= printResult . fmap countBlockTiles . buildGameGrid . startGame

solutionPart2 :: IO ()
solutionPart2 = readInputData inputFile >>= printResult . fmap score . playGame . startFreeGame
