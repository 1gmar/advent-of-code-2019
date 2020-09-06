module Day10
  ( solutionPart1,
    solutionPart2,
  )
where

import Data.List (maximumBy, sortBy, (\\))
import Util.ParseUtils hiding (count)

data Cell
  = Empty
  | Asteroid
  deriving (Show)

type Row = [Cell]

type AsteroidMap = [Position]

type Position = (Int, Int)

positionDeltaFor :: Position -> Position -> (Int, Int)
positionDeltaFor (rx, ry) (tx, ty) = (x `div` divisor, y `div` divisor)
  where
    x = abs (tx - rx)
    y = abs (ty - ry)
    divisor = x `gcd` y

laserAngle :: Position -> Position -> Double
laserAngle (rx, ry) (x, y) = atan2 (fromIntegral (ry - y)) (fromIntegral (x - rx))

inSightFor :: AsteroidMap -> Position -> Position -> Bool
inSightFor asteroidMap reference@(rx, ry) target@(tx, ty)
  | (rx <= tx) && (ry <= ty) = not . any (`elem` asteroidMap) $ traverseWith (<=) (<=) (-) (-)
  | (rx <= tx) && (ry > ty) = not . any (`elem` asteroidMap) $ traverseWith (<=) (>) (-) (+)
  | (rx > tx) && (ry <= ty) = not . any (`elem` asteroidMap) $ traverseWith (>) (<=) (+) (-)
  | otherwise = not . any (`elem` asteroidMap) $ traverseWith (>) (>) (+) (+)
  where
    traverseWith xCmp yCmp xOp yOp = takeWhile (notRef xCmp yCmp) $ iterate (advance xOp yOp) (advance xOp yOp target)
    notRef xCmp yCmp (x, y) = rx `xCmp` x && ry `yCmp` y
    advance xOp yOp (x, y) = (x `xOp` dx, y `yOp` dy)
    (dx, dy) = positionDeltaFor reference target

detectAsteroidsFrom :: AsteroidMap -> Position -> (Position, Int)
detectAsteroidsFrom asteroidMap reference = (reference, foldl countAsteroids 0 withoutRefMap)
  where
    withoutRefMap = filter (/= reference) asteroidMap
    countAsteroids count target
      | inSightFor (filter (/= target) withoutRefMap) reference target = count + 1
      | otherwise = count

findBestPosition :: AsteroidMap -> (Position, Int)
findBestPosition asteroidMap = maximumBy comparePos $ map (detectAsteroidsFrom asteroidMap) asteroidMap
  where
    comparePos (_, count1) (_, count2) = count1 `compare` count2

findVaporizedAsteroidsFrom :: Position -> AsteroidMap -> AsteroidMap
findVaporizedAsteroidsFrom reference asteroidMap = filter inSightOnly asteroidMap
  where
    inSightOnly target = inSightFor (filter (/= target) asteroidMap) reference target

sortClockWise :: Position -> AsteroidMap -> AsteroidMap
sortClockWise ref@(rx, _) asteroidMap = headMap ++ tailMap
  where
    (tailMap, headMap) = span (\(x, _) -> x < rx) sortedMap
    sortedMap = sortBy clockWise asteroidMap
    clockWise pos1 pos2 = laserAngle ref pos2 `compare` laserAngle ref pos1

allLaserRotations :: Position -> AsteroidMap -> AsteroidMap
allLaserRotations _ [] = []
allLaserRotations reference asteroidMap = rotation ++ allLaserRotations reference (asteroidMap \\ rotation)
  where
    rotation = (sortClockWise reference . findVaporizedAsteroidsFrom reference) asteroidMap

find200thVaporizedAsteroid :: AsteroidMap -> Position
find200thVaporizedAsteroid asteroidMap = get200thElem vaporizedAsteroids
  where
    laserPos = fst $ findBestPosition asteroidMap
    vaporizedAsteroids = (allLaserRotations laserPos . filter (/= laserPos)) asteroidMap
    get200thElem = last . take 200

inputParser :: ReadP [Row]
inputParser = trimSpacesEOF $ rows `sepBy` endOfLine
  where
    rows = many1 cell
    cell = readCell <$> choice [char '#', char '.']
    readCell '#' = Asteroid
    readCell _ = Empty

parseAsteroids :: String -> AsteroidMap
parseAsteroids input = concatRows rows
  where
    rows = parseInput inputParser input
    width = rowSize rows
    concatRows = fst . foldl (collectAsteroids width) ([], 0) . concat
    rowSize [] = 0
    rowSize (row : _) = length row

collectAsteroids :: Int -> (AsteroidMap, Int) -> Cell -> (AsteroidMap, Int)
collectAsteroids width (asteroids, index) = \case
  Asteroid -> ((x, y) : asteroids, index + 1)
  Empty -> (asteroids, index + 1)
  where
    y = index `div` width
    x = index - y * width

solutionPart1 :: String -> Int
solutionPart1 = snd . findBestPosition . parseAsteroids

solutionPart2 :: String -> Int
solutionPart2 = posChecksum . find200thVaporizedAsteroid . parseAsteroids
  where
    posChecksum (x, y) = 100 * x + y
