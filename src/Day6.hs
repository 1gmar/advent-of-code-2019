module Day6
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Char                    (isAsciiUpper, isDigit)
import           Text.ParserCombinators.ReadP (ReadP, char, eof, munch, readP_to_S, skipSpaces)

data OrbitTree =
  SpaceObject String [OrbitTree]

type OrbitalTransfer = (String, String)

type OrbitMap = [(String, String)]

type ChecksumTable = [(String, Int)]

type SpaceRoute = [String]

buildOrbitTree :: OrbitMap -> String -> OrbitTree
buildOrbitTree orbitMap name = SpaceObject name objectsInOrbit
  where
    objectsInOrbit = map (buildOrbitTree orbitMap . snd) inOrbitObjectsMap
    inOrbitObjectsMap = filter ((name ==) . fst) orbitMap

computeOrbitChecksum :: OrbitTree -> ChecksumTable
computeOrbitChecksum = snd . countOrbitsFor . pure
  where
    countOrbitsFor = foldr sumAndCache (0, [])
    sumAndCache (SpaceObject name objectsInOrbit) (count, cache) =
      let (inOrbitCount, inOrbitCache) = countOrbitsFor objectsInOrbit
       in (count + inOrbitCount + 1, (name, inOrbitCount) : cache ++ inOrbitCache)

countAllOrbits :: OrbitTree -> Int
countAllOrbits = sum . map snd . computeOrbitChecksum

allObjectRoutes :: OrbitTree -> [SpaceRoute]
allObjectRoutes = collectAllObjectRoutes . pure
  where
    collectAllObjectRoutes = foldr concatRoutes [[]]
    concatRoutes (SpaceObject name objectsInOrbit) routes =
      let inOrbitRoutes = collectAllObjectRoutes objectsInOrbit
       in map (name :) inOrbitRoutes ++ filter (not . null) routes

routeToSpaceObject :: String -> OrbitTree -> SpaceRoute
routeToSpaceObject targetObject = concat . filter (targetObject `elem`) . allObjectRoutes

nearestCommonOrbitedObject :: (SpaceRoute, SpaceRoute) -> String
nearestCommonOrbitedObject (routeToYou, routeToSanta) =
  let zippedRoutes = routeToYou `zip` routeToSanta
      commonOrbitedObjects = map fst $ takeWhile (uncurry (==)) zippedRoutes
   in case commonOrbitedObjects of
        []      -> "COM"
        objects -> last objects

minOrbitalTransfers :: (String, String) -> OrbitTree -> Int
minOrbitalTransfers (you, santa) orbitTree = countTransfersFor routeToYou + countTransfersFor routeToSanta
  where
    routeToYou = routeToSpaceObject you orbitTree
    routeToSanta = routeToSpaceObject santa orbitTree
    nearestCommonObject = nearestCommonOrbitedObject (routeToYou, routeToSanta)
    countTransfersFor = subtract 1 . length . toTransfers . dropWhile (/= nearestCommonObject)

toTransfers :: SpaceRoute -> [OrbitalTransfer]
toTransfers route = route `zip` tail route

inputParser :: ReadP (String, String)
inputParser = skipSpaces *> parenSeparatedStrings <* skipSpaces <* eof
  where
    parenSeparatedStrings = (,) <$> munch alphaNumUpper <* char ')' <*> munch alphaNumUpper
    alphaNumUpper c = isAsciiUpper c || isDigit c

parseInput :: String -> OrbitMap
parseInput = map fst . readP_to_S inputParser

readInput :: IO OrbitMap
readInput = concatMap parseInput . lines <$> readFile "./resources/input-day6.txt"

solutionPart1 :: IO Int
solutionPart1 = countAllOrbits . flip buildOrbitTree "COM" <$> readInput

solutionPart2 :: IO Int
solutionPart2 = minOrbitalTransfers ("YOU", "SAN") . flip buildOrbitTree "COM" <$> readInput
