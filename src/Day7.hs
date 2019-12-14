{-# LANGUAGE RecordWildCards #-}

module Day7
  ( solutionPart1
  , solutionPart2
  ) where

import           Control.Monad                (foldM)
import           Data.Char                    (digitToInt, isDigit)
import           Data.List                    (maximumBy, permutations)
import           Data.Traversable             (for)
import           Text.ParserCombinators.ReadP (ReadP, char, eof, munch, readP_to_S, sepBy, skipSpaces, (+++))

data ParamMode
  = Position
  | Immediate

data Operation
  = Arithmetic Operator
  | Input
  | Output
  | JumpIf Predicate
  | Test Assertion

data ProgramState =
  ProgramState
    { iPointer :: Int
    , input    :: [Int]
    , program  :: [String]
    , result   :: Int
    , halted   :: Bool
    }

data Instruction =
  Instruction
    { operation :: Operation
    , params    :: [Parameter]
    }

data Amplifier =
  Amplifier
    { label     :: Char
    , softState :: ProgramState
    }

type ProgramOutput = Either String ProgramState

type Parameter = (ParamMode, Int)

type Operator = Int -> Int -> Int

type Assertion = Int -> Int -> Bool

type Predicate = Int -> Bool

type AmpChainRunner = [String] -> [Int] -> ProgramOutput

toParamMode :: Int -> Either String ParamMode
toParamMode mode =
  case mode of
    0 -> Right Position
    1 -> Right Immediate
    _ -> Left $ "Unknown parameter mode: " ++ show mode

toOperation :: Char -> Either String Operation
toOperation opCode =
  case opCode of
    '1' -> Right $ Arithmetic (+)
    '2' -> Right $ Arithmetic (*)
    '3' -> Right Input
    '4' -> Right Output
    '5' -> Right $ JumpIf (/= 0)
    '6' -> Right $ JumpIf (== 0)
    '7' -> Right $ Test (<)
    '8' -> Right $ Test (==)
    _   -> Left $ "Unknown operation code: " ++ show opCode

nextIPointer :: Operation -> Int -> Int
nextIPointer op =
  case op of
    Arithmetic _ -> (+ 4)
    Input        -> (+ 2)
    Output       -> (+ 2)
    JumpIf _     -> (+ 3)
    Test _       -> (+ 4)

programWith :: Int -> Int -> [String] -> [String]
programWith = flip replaceAt . show

replaceAt :: Int -> String -> [String] -> [String]
replaceAt pos value list =
  let (upper, lower) = splitAt pos list
   in upper ++ value : drop 1 lower

elemAt :: [String] -> Int -> Either String String
elemAt list index
  | 0 <= index && index < length list = Right $ list !! index
  | otherwise = Left $ "Index out of bounds: " ++ show index

paramOf :: Parameter -> [String] -> Either String Int
paramOf (Position, param)  = fmap read . flip elemAt param
paramOf (Immediate, param) = const $ Right param

endOfProgram :: [String] -> Int -> Bool
endOfProgram program = elem "99" . elemAt program

readInputSequence :: ProgramState -> Instruction -> ProgramOutput
readInputSequence ProgramState {..} (Instruction _ []) = illegalProgramState iPointer program
readInputSequence state@ProgramState {..} (Instruction op ((_, resultP):_)) =
  case input of
    param:inputLeft -> runIntCodeProgram $ state {iPointer = nextP, input = inputLeft, program = nextProgram param}
    _               -> Left "Missing program input data."
  where
    nextP = nextIPointer op iPointer
    nextProgram param = programWith param resultP program

outputSignal :: ProgramState -> Instruction -> ProgramOutput
outputSignal state@ProgramState {..} instr@Instruction {..} =
  case instr of
    Instruction _ (param:_) -> paramOf param program >>= Right . nextState
    _                       -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    nextState signal = state {iPointer = nextP, result = signal, halted = endOfProgram program nextP}

jumpToPointerIf :: ProgramState -> Instruction -> Predicate -> ProgramOutput
jumpToPointerIf state@ProgramState {..} instr@Instruction {..} predicate =
  case instr of
    Instruction _ (param1:param2:_) -> paramOf param1 program >>= chooseIPointer param2 >>= runProgramWith
    _                               -> illegalProgramState iPointer program
  where
    runProgramWith pointer = runIntCodeProgram state {iPointer = pointer}
    chooseIPointer param value
      | predicate value = paramOf param program
      | otherwise = Right $ nextIPointer operation iPointer

testAssertion :: ProgramState -> Instruction -> Assertion -> ProgramOutput
testAssertion state@ProgramState {..} instr@Instruction {..} predicate =
  case instr of
    Instruction _ (param1:param2:(_, resultP):_) -> do
      arg1 <- paramOf param1 program
      arg2 <- paramOf param2 program
      runIntCodeProgram $ nextState resultP (arg1, arg2)
    _ -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    nextState resultP args = state {iPointer = nextP, program = programWith (chooseResult args) resultP program}
    chooseResult (arg1, arg2)
      | arg1 `predicate` arg2 = 1
      | otherwise = 0

computeValue :: ProgramState -> Instruction -> Operator -> ProgramOutput
computeValue state@ProgramState {..} instr@Instruction {..} operator =
  case instr of
    Instruction _ (param1:param2:(_, resultP):_) -> do
      value <- operator <$> paramOf param1 program <*> paramOf param2 program
      runIntCodeProgram state {iPointer = nextP, program = programWith value resultP program}
    _ -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer

illegalProgramState :: Int -> [String] -> ProgramOutput
illegalProgramState iPointer program = Left $ "Illegal program state at: " ++ show (iPointer, program)

runInstruction :: ProgramState -> Instruction -> ProgramOutput
runInstruction state instr@Instruction {..} =
  case operation of
    Input            -> readInputSequence state instr
    Output           -> outputSignal state instr
    JumpIf predicate -> jumpToPointerIf state instr predicate
    Test assert      -> testAssertion state instr assert
    Arithmetic op    -> computeValue state instr op

buildInstruction :: String -> [String] -> Either String Instruction
buildInstruction instrCode args =
  case instrCode of
    [opChar]                           -> instructionWith '0' '0' opChar
    '0':[opChar]                       -> instructionWith '0' '0' opChar
    paramMode:'0':[opChar]             -> instructionWith paramMode '0' opChar
    paramMode2:paramMode1:'0':[opChar] -> instructionWith paramMode1 paramMode2 opChar
    _                                  -> Left $ "Unknown instruction: " ++ show instrCode
  where
    instructionWith paramMode1 paramMode2 opChar = do
      mode1 <- toParamMode $ digitToInt paramMode1
      mode2 <- toParamMode $ digitToInt paramMode2
      op <- toOperation opChar
      let params = zipWith (\mode param -> (mode, read param)) [mode1, mode2, Position] args
      Right $ Instruction op params

runIntCodeProgram :: ProgramState -> ProgramOutput
runIntCodeProgram state@ProgramState {..}
  | endOfProgram program iPointer = Right state {halted = True}
  | otherwise = processInstruction $ drop iPointer program
  where
    processInstruction [] = Left "Program reached end of input!"
    processInstruction (instr:args)
      | any (`elem` "34") instr = buildInstruction instr (take 1 args) >>= runInstruction state
      | any (`elem` "56") instr = buildInstruction instr (take 2 args) >>= runInstruction state
      | any (`elem` "1278") instr = buildInstruction instr (take 3 args) >>= runInstruction state
      | otherwise = Left $ "Invalid instruction : " ++ show instr

runSimpleChain :: [String] -> [Int] -> ProgramOutput
runSimpleChain initProg = foldM chainResult (ProgramState 0 [] [] 0 False)
  where
    chainResult ProgramState {..} phase = runIntCodeProgram $ ProgramState 0 [phase, result] initProg 0 False

findMaxPossibleSignal :: AmpChainRunner -> [[Int]] -> [String] -> ProgramOutput
findMaxPossibleSignal runner allPhaseSeq = fmap (maximumBy compareStates) . for allPhaseSeq . runner
  where
    compareStates (ProgramState _ _ _ res1 _) (ProgramState _ _ _ res2 _) = res1 `compare` res2

runLoopModeChain :: [String] -> [Int] -> ProgramOutput
runLoopModeChain initProg phaseSetting = loopOver amplifiers
  where
    amplifiers = zipWith buildAmplifier ['A' .. 'E'] phaseSetting
    buildAmplifier label phase = Amplifier label $ ProgramState 0 [phase] initProg 0 False

loopOver :: [Amplifier] -> ProgramOutput
loopOver [] = Left "Missing amplifier chain."
loopOver [_] = Left "Illegal amplifier setup."
loopOver (current:next@Amplifier {..}:rest) =
  case current of
    Amplifier 'E' state@(ProgramState _ _ _ _ True)    -> Right state
    amp@(Amplifier 'A' state@(ProgramState 0 _ _ _ _)) -> runIntCodeProgram (inSignal 0 state) >>= chainResult amp
    amp@(Amplifier _ state)                            -> runIntCodeProgram state >>= chainResult amp
  where
    chainResult amp state@(ProgramState _ _ _ res _) = loopOver (setupNext res : rest ++ [amp {softState = state}])
    setupNext res = next {softState = inSignal res softState}
    inSignal signal state@ProgramState {..} = state {input = input ++ [signal]}

inputParser :: ReadP [String]
inputParser = skipSpaces *> commaSeparatedIntegers <* skipSpaces <* eof
  where
    commaSeparatedIntegers = sepBy integer (char ',')
    integer = positive +++ negative
    positive = munch isDigit
    negative = char '-' >>= \sign -> (sign :) <$> positive

parseInput :: String -> [String]
parseInput = concatMap fst . readP_to_S inputParser

readInput :: IO [String]
readInput = parseInput <$> readFile "./resources/input-day7.txt"

showProgramOutput :: ProgramOutput -> String
showProgramOutput (Left err)    = "Error: " ++ err
showProgramOutput (Right state) = show $ result state

solutionPart1 :: IO ()
solutionPart1 =
  readInput >>= putStrLn . showProgramOutput . findMaxPossibleSignal runSimpleChain (permutations [0 .. 4])

solutionPart2 :: IO ()
solutionPart2 =
  readInput >>= putStrLn . showProgramOutput . findMaxPossibleSignal runLoopModeChain (permutations [5 .. 9])
