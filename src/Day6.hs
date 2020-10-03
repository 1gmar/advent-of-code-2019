module Day6
  ( solutionPart1,
    solutionPart2,
  )
where

import Data.Char (isAsciiUpper, isDigit)
import Data.List (foldl')
import Util.ParseUtils hiding (count)

data OrbitTree = SpaceObject String [OrbitTree]

type OrbitalTransfer = (String, String)

type OrbitMap = [(String, String)]

type ChecksumTable = [(String, Int)]

type SpaceRoute = [String]

buildOrbitTree :: String -> OrbitMap -> OrbitTree
buildOrbitTree name orbitMap = SpaceObject name objectsInOrbit
  where
    objectsInOrbit = map ((`buildOrbitTree` orbitMap) . snd) inOrbitObjectsMap
    inOrbitObjectsMap = filter ((name ==) . fst) orbitMap

computeOrbitChecksum :: OrbitTree -> ChecksumTable
computeOrbitChecksum = snd . countOrbitsFor . pure
  where
    countOrbitsFor = foldl' sumAndCache (0, [])
    sumAndCache (count, cache) (SpaceObject name objectsInOrbit) =
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
        [] -> "COM"
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

inputParser :: ReadP OrbitMap
inputParser = trimSpacesEOF $ line `sepBy` endOfLine
  where
    line = (,) <$> object <* char ')' <*> object
    object = munch1 alphaNumUpper
    alphaNumUpper c = isAsciiUpper c || isDigit c

solutionPart1 :: String -> Int
solutionPart1 = countAllOrbits . buildOrbitTree "COM" . parseInput inputParser

solutionPart2 :: String -> Int
solutionPart2 = minOrbitalTransfers ("YOU", "SAN") . buildOrbitTree "COM" . parseInput inputParser
