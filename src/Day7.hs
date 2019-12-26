{-# LANGUAGE RecordWildCards #-}

module Day7
  ( solutionPart1
  , solutionPart2
  ) where

import           Control.Monad    (foldM)
import           Data.List        (maximumBy, permutations)
import           Data.Traversable (for)
import           IntCodeProgram

type AmpChainRunner = [String] -> [Int] -> ProgramResult

data Amplifier =
  Amplifier
    { label     :: Char
    , softState :: ProgramState
    }

runSimpleChain :: [String] -> [Int] -> ProgramResult
runSimpleChain initProg = foldM chainResult (programState initProg)
  where
    chainResult ProgramState {..} phase = runIntCodeProgram $ programWithInput initProg [phase, result]

findMaxPossibleSignal :: AmpChainRunner -> [[Int]] -> [String] -> ProgramResult
findMaxPossibleSignal runner allPhaseSeq = fmap (maximumBy compareStates) . for allPhaseSeq . runner
  where
    compareStates state1 state2 = result state1 `compare` result state2

runLoopModeChain :: [String] -> [Int] -> ProgramResult
runLoopModeChain initProg phaseSetting = loopOver amplifiers
  where
    amplifiers = zipWith buildAmplifier ['A' .. 'E'] phaseSetting
    buildAmplifier label phase = Amplifier label $ programWithInput initProg [phase]

loopOver :: [Amplifier] -> ProgramResult
loopOver [] = Left "Missing amplifier chain."
loopOver [_] = Left "Illegal amplifier setup."
loopOver (current@(Amplifier code state):next@Amplifier {..}:rest)
  | code == 'E' && halted state = Right state
  | code == 'A' && iPointer state == 0 = runIntCodeProgram (inSignal 0 state) >>= chainResult current
  | otherwise = runIntCodeProgram state >>= chainResult current
  where
    chainResult amp nextState = loopOver (setupNext (result nextState) : rest ++ [amp {softState = nextState}])
    setupNext res = next {softState = inSignal res softState}
    inSignal signal nextState@ProgramState {..} = nextState {input = input ++ [signal]}

inputFile :: String
inputFile = "./resources/input-day7.txt"

solutionPart1 :: IO ()
solutionPart1 =
  readInputData inputFile >>= putStrLn . showResult . findMaxPossibleSignal runSimpleChain (permutations [0 .. 4])

solutionPart2 :: IO ()
solutionPart2 =
  readInputData inputFile >>= putStrLn . showResult . findMaxPossibleSignal runLoopModeChain (permutations [5 .. 9])
