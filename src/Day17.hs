{-# LANGUAGE DeriveAnyClass #-}

module Day17
  ( solutionPart1,
    solutionPart2,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Char (chr, ord)
import Data.List (foldl', inits, intercalate, intersperse)
import Data.Maybe (listToMaybe)
import Data.Vector (Vector, empty, find, fromList, generate, indexed, snoc, toList, (!), (!?))
import qualified Data.Vector as V (concat, drop, init, length, reverse, takeWhile)
import Util.CyclicEnumClass
import Util.IntCodeProgram
import Util.ParseUtils

type View = Vector (Vector Char)

data Rotation
  = RIGHT
  | LEFT
  deriving (Eq, Enum, Bounded)

instance Show Rotation where
  show RIGHT = "R"
  show LEFT = "L"

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Enum, Bounded, CyclicEnum)

data Robot = Robot
  { direction :: Direction,
    row :: Int,
    col :: Int,
    view :: View,
    height :: Int,
    width :: Int,
    path :: [String]
  }

data ABCPath = ABCPath
  { mainRoutine :: String,
    routineA :: String,
    routineB :: String,
    routineC :: String
  }
  deriving (Show)

scaffold :: Char
scaffold = '#'

(!.!) :: View -> (Int, Int) -> Char
(!.!) view (i, j) = view ! i ! j

toDirection :: Char -> Direction
toDirection dirCode
  | dirCode == '<' = West
  | dirCode == '>' = East
  | dirCode == 'v' = South
  | otherwise = North

toPosition :: Int -> Int -> (Int, Int)
toPosition index width = (row, col)
  where
    row = index `div` width
    col = index - row * width

parseViewList :: [Int] -> View
parseViewList = foldl' collectRows empty . filter (not . null) . lines . map chr
  where
    collectRows view row = view `snoc` fromList row

findIntersections :: View -> [(Int, Int)]
findIntersections view = foldl' findForRow [] [1 .. height - 2]
  where
    height = V.length view
    width = maybe 0 V.length (view !? 0)
    isIntersection i j = all (== scaffold) [view !.! pos | pos <- [(i - 1, j), (i, j - 1), (i, j + 1), (i + 1, j)]]
    findForRow iSections row = foldl' (checkCellAt row) iSections [1 .. width - 2]
    checkCellAt i iSections j
      | view !.! (i, j) == scaffold && isIntersection i j = (i, j) : iSections
      | otherwise = iSections

sumAlignParams :: ProgramState -> Either String Int
sumAlignParams = fmap (sumParams . findIntersections . parseViewList . outputList) . runIntCodeProgram
  where
    sumParams = sum . map (uncurry (*))

initRobot :: View -> Either String Robot
initRobot view =
  case toRobot <$> find isRobot (flattenView view) of
    Just robot -> return robot
    Nothing -> throwError "Robot character not found in view!"
  where
    width = maybe 0 V.length (view !? 0)
    isRobot = flip elem ['<', '^', '>', 'v'] . snd
    flattenView = indexed . V.concat . toList
    toRobot (index, code) =
      let (row, col) = toPosition index width
       in Robot
            { direction = toDirection code,
              row = row,
              col = col,
              view = view,
              height = V.length view,
              width = width,
              path = []
            }

rotateRobot :: Robot -> Maybe Robot
rotateRobot robot@Robot {..} =
  listToMaybe [robot {direction = rotate rot, path = show rot : path} | rot <- enumFrom RIGHT, onScaffold (rotate rot)]
  where
    isScaffold pos = view !.! pos == scaffold
    rotate = \case
      RIGHT -> cSucc direction
      LEFT -> cPred direction
    onScaffold = \case
      North -> row > 0 && isScaffold (row - 1, col)
      East -> col < width - 1 && isScaffold (row, col + 1)
      South -> row < height - 1 && isScaffold (row + 1, col)
      West -> col > 0 && isScaffold (row, col - 1)

pathVector :: Robot -> Vector Char
pathVector Robot {..} =
  case direction of
    North -> V.drop (height - row) $ V.reverse (column col)
    East -> V.drop (col + 1) $ view ! row
    South -> V.drop (row + 1) $ column col
    West -> V.drop (width - col) $ V.reverse (view ! row)
  where
    column j = generate height (getColumn j)
    getColumn j i = view !.! (i, j)

moveRobot :: Robot -> Robot
moveRobot robot@Robot {..} =
  let coveredPath = V.takeWhile (== scaffold) $ pathVector robot
      distance = V.length coveredPath
      (i, j) = nextPosition distance
   in robot {row = i, col = j, path = show distance : path}
  where
    nextPosition distance =
      case direction of
        North -> (row - distance, col)
        East -> (row, col + distance)
        South -> (row + distance, col)
        West -> (row, col - distance)

findRobotPath :: Robot -> [String]
findRobotPath robot =
  case rotateRobot robot of
    Just rotatedRobot -> findRobotPath $ moveAndSortPath rotatedRobot
    Nothing -> path robot
  where
    moveAndSortPath rotatedRobot =
      let robot' = moveRobot rotatedRobot
          distance : rotation : sortedPath = path robot'
       in robot' {path = sortedPath ++ [concat [rotation, ",", distance]]}

groupBySize :: Int -> [a] -> [[a]]
groupBySize size list
  | null list = []
  | size == 0 = [list]
  | otherwise =
    let (xs, rest) = splitAt size list
     in xs : groupBySize size rest

pathParser :: (String, String, String) -> ReadP String
pathParser (pathA, pathB, pathC) = (++) <$> parserA <*> (concat <$> pathPattern) <* eof
  where
    pathPattern = many1 $ choice [parserA, parserB, parserC]
    parserA = "A" <$ string pathA
    parserB = "B" <$ string pathB
    parserC = "C" <$ string pathC

attemptABCPath :: [String] -> ([String], [String], [String]) -> ABCPath
attemptABCPath fullPath (a, b, c) =
  ABCPath
    { mainRoutine = intersperse ',' parseForInputPath,
      routineA = insertCommas a,
      routineB = insertCommas b,
      routineC = insertCommas c
    }
  where
    insertCommas = intercalate ","
    concatTuple = (concat a, concat b, concat c)
    parseForInputPath = parseInput (pathParser concatTuple) (concat fullPath)

reducePath :: [String] -> Either String ABCPath
reducePath fullPath =
  case listToMaybe (parseAttempts substringTuples) of
    Just abcPath -> return abcPath
    Nothing -> throwError $ "Unable to reduce path for: " ++ concat fullPath
  where
    notEmpty = not . null
    parseAttempts = filter (notEmpty . mainRoutine) . map (attemptABCPath fullPath)
    withinBounds a b c = all ((<= 5) . length) [a, b, c]
    filterOccurrences path = concat . filter (/= path) . groupBySize (length path)
    substringTuples =
      [ (a, b, c)
        | a <- inits fullPath,
          notEmpty a,
          b <- inits $ filterOccurrences a fullPath,
          notEmpty b,
          c <- (inits . filterOccurrences b . filterOccurrences a) fullPath,
          notEmpty c,
          withinBounds a b c
      ]

runVacuumRobot :: ProgramState -> Either String Int
runVacuumRobot initState = do
  state <- runIntCodeProgram initState
  let view = (V.init . parseViewList . outputList) state
  fullPath <- findRobotPath <$> initRobot view
  abcPath <- reducePath fullPath
  result <$> runIntCodeProgram state {input = movementRoutines abcPath}
  where
    movementRoutines ABCPath {..} = concatMap asciiCode [mainRoutine, routineA, routineB, routineC, "n"]
    asciiCode = map ord . (++ "\n")

wakeUpState :: [Int] -> ProgramState
wakeUpState prog = newProgram (2 : drop 1 prog)

solutionPart1 :: String -> Either String Int
solutionPart1 = sumAlignParams . newProgram . parseIntCode

solutionPart2 :: String -> Either String Int
solutionPart2 = runVacuumRobot . wakeUpState . parseIntCode
