module Day8
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.List  (minimumBy)
import           ParseUtils

data Pixel
  = Black
  | White
  | Transparent
  deriving (Eq)

type Image = [Layer]

type Layer = [Pixel]

width :: Int
width = 25

height :: Int
height = 6

layerLength :: Int
layerLength = width * height

countPixel :: Pixel -> Layer -> Int
countPixel pixel = length . filter (== pixel)

layerWithMinWhite :: Image -> Layer
layerWithMinWhite = minimumBy compareLayers
  where
    compareLayers lhs rhs = countPixel White lhs `compare` countPixel White rhs

blackTimesTransparent :: Layer -> Int
blackTimesTransparent layer = countPixel Black layer * countPixel Transparent layer

zipImageLayers :: Image -> Layer
zipImageLayers = foldl zipLayers (repeat Transparent)
  where
    zipLayers = zipWith pixelZipper
    pixelZipper Transparent p = p
    pixelZipper p _           = p

showLayer :: Layer -> String
showLayer = unlines . collectRows
  where
    collectRows []     = []
    collectRows digits = map colorPixel (take width digits) : collectRows (drop width digits)
    colorPixel pixel =
      case pixel of
        Black -> '⬛'
        _     -> '⬜'

readImage :: [Pixel] -> Image
readImage []      = []
readImage rawData = take layerLength rawData : readImage (drop layerLength rawData)

inputParser :: ReadP String
inputParser = trimSpacesEOF digits
  where
    digits = many1 $ choice [char '0', char '1', char '2']

parseInput :: String -> [Pixel]
parseInput = map readColor . concatMap fst . readP_to_S inputParser
  where
    readColor digit =
      case digit of
        '0' -> White
        '1' -> Black
        _   -> Transparent

readInput :: IO Image
readInput = readImage . parseInput <$> readFile "./resources/input-day8.txt"

solutionPart1 :: IO Int
solutionPart1 = blackTimesTransparent . layerWithMinWhite <$> readInput

solutionPart2 :: IO ()
solutionPart2 = readInput >>= writeFile "./out/output-day8.txt" . showLayer . zipImageLayers
