module Day14
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Bifunctor (bimap)
import           Data.Char      (isAsciiUpper)
import           Data.List      (find, groupBy, sortOn, (\\))
import           Data.Maybe     (listToMaybe)
import           Util.ParseUtils

type Element = (String, Int)

type RecipeMap = [(Element, [Element])]

multiplier :: Int -> Int -> Int
multiplier cost batchSize
  | cost <= batchSize = 1
  | cost `mod` batchSize == 0 = cost `div` batchSize
  | otherwise = cost `div` batchSize + 1

findByLabel :: String -> RecipeMap -> Maybe (Element, [Element])
findByLabel label = find ((== label) . fst . fst)

getFuelRecipe :: RecipeMap -> Maybe [Element]
getFuelRecipe = fmap snd . findByLabel "FUEL"

updateCost :: Int -> Element -> Element
updateCost mult = bimap id (* mult)

addUpCosts :: [Element] -> [Element]
addUpCosts = map (foldl1 elementCost) . groupBy compareLabels . sortOn fst
  where
    compareLabels elem1 elem2 = fst elem1 == fst elem2
    elementCost (label, totalCost) (_, cost) = (label, totalCost + cost)

reuseSurplus :: RecipeMap -> [Element] -> Int
reuseSurplus recipeMap surplus =
  case listToMaybe reuseCandidates of
    Just element -> reuseSurplus recipeMap (updateSurplus element)
    Nothing      -> maybe 0 snd $ find ((== "ORE") . fst) totals
  where
    totals = addUpCosts surplus
    spareLeft spareQty bSize = spareQty - bSize * (spareQty `div` bSize)
    mapCost spareQty bSize = map (updateCost (spareQty `div` bSize))
    updateSurplus (label, spareQty, bSize, recipe) =
      (label, spareLeft spareQty bSize) : mapCost spareQty bSize recipe ++ (totals \\ [(label, spareQty)])
    reuseCandidates =
      [ (label, spareQty, batchSize, recipe)
      | (label, spareQty) <- totals
      , ((code, batchSize), recipe) <- recipeMap
      , code == label
      , spareQty >= batchSize
      ]

stepInRecipe :: RecipeMap -> Element -> ([Element], Element)
stepInRecipe recipeMap (label, cost) = (map (updateCost mult) recipe, surplus)
  where
    (batchSize, recipe) = maybe (cost, [(label, cost)]) batchSizeToRecipe element
    element = findByLabel label recipeMap
    mult = multiplier cost batchSize
    batchSizeToRecipe = bimap snd id
    surplus = (label, mult * batchSize - cost)

computeFuelCost :: RecipeMap -> [Element] -> [Element] -> Int
computeFuelCost _ _ [] = 0
computeFuelCost recipeMap surplus recipe =
  case recipe of
    [(_, ore)] -> ore - reusedOre
    _          -> computeFuelCost recipeMap newSurplus recipes
  where
    reusedOre = reuseSurplus recipeMap surplus
    recipeToSurplus = map (stepInRecipe recipeMap) recipe
    recipes = (addUpCosts . concatMap fst) recipeToSurplus
    newSurplus = map snd recipeToSurplus ++ surplus

computeMinCost :: RecipeMap -> Maybe Int
computeMinCost recipeMap = computeFuelCost recipeMap [] <$> getFuelRecipe recipeMap

maxFuelFor :: Int -> RecipeMap -> (Int, Int, Int) -> [Element] -> Int
maxFuelFor cargoOre recipeMap (low, high, maxFuel) recipe
  | low > high = maxFuel
  | requiredOre > cargoOre = maxFuelFor cargoOre recipeMap (low, trialFuel - 1, maxFuel) recipe
  | otherwise = maxFuelFor cargoOre recipeMap (trialFuel + 1, high, trialFuel) recipe
  where
    trialFuel = (low + high) `div` 2
    trialRecipe = map (updateCost trialFuel) recipe
    requiredOre = computeFuelCost recipeMap [] trialRecipe

findMaxFuelFor :: Int -> RecipeMap -> Maybe Int
findMaxFuelFor cargoOre recipeMap = do
  recipe <- getFuelRecipe recipeMap
  let orePerFuel = computeFuelCost recipeMap [] recipe
  return $ maxFuelFor cargoOre recipeMap (cargoOre `div` orePerFuel, cargoOre, 0) recipe

inputParser :: ReadP RecipeMap
inputParser = trimSpacesEOF $ line `sepBy` endOfLine
  where
    line = flip (,) <$> recipe <*> (skipSpaces *> string "=>" *> element)
    recipe = element `sepBy` char ','
    element = flip (,) <$> (skipSpaces *> integer) <*> (skipSpaces *> code)
    code = munch1 isAsciiUpper

solutionPart1 :: String -> Maybe Int
solutionPart1 = computeMinCost . parseInput inputParser

solutionPart2 :: String -> Maybe Int
solutionPart2 = findMaxFuelFor 1000000000000 . parseInput inputParser
