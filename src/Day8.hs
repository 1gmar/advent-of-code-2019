module Day8
  ( solutionPart1,
    solutionPart2,
  )
where

import Data.List (foldl', minimumBy)
import Util.ParseUtils

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
zipImageLayers = foldl' zipLayers (repeat Transparent)
  where
    zipLayers = zipWith pixelZipper
    pixelZipper Transparent p = p
    pixelZipper p _ = p

showLayer :: Layer -> String
showLayer = unlines . collectRows
  where
    collectRows [] = []
    collectRows digits = map colorPixel (take width digits) : collectRows (drop width digits)
    colorPixel = \case
      Black -> '\x2B1B'
      _ -> '\x2B1C'

readImage :: [Pixel] -> Image
readImage [] = []
readImage rawData = take layerLength rawData : readImage (drop layerLength rawData)

inputParser :: ReadP [Pixel]
inputParser = trimSpacesEOF digits
  where
    digits = many1 $ readColor <$> choice [char '0', char '1', char '2']
    readColor = \case
      '0' -> White
      '1' -> Black
      _ -> Transparent

parsePixels :: String -> Image
parsePixels = readImage . parseInput inputParser

solutionPart1 :: String -> Int
solutionPart1 = blackTimesTransparent . layerWithMinWhite . parsePixels

solutionPart2 :: String -> String
solutionPart2 = showLayer . zipImageLayers . parsePixels
