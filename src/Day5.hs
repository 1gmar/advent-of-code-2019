{-# LANGUAGE RecordWildCards #-}

module Day5
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Char                    (digitToInt, isDigit)
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

data ParamData =
  ParamData
    { mode  :: ParamMode
    , param :: Int
    }

data ProgramState =
  ProgramState
    { iPointer :: Int
    , input    :: [Int]
    , program  :: [String]
    }

data Instruction =
  Instruction
    { operation :: Operation
    , params    :: Params
    }

type ProgramOutput = Either String [String]

type Params = [(ParamMode, Int)]

type Operator = Int -> Int -> Int

type Assertion = Int -> Int -> Bool

type Predicate = Int -> Bool

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

runInstruction :: ProgramState -> Instruction -> ProgramOutput
runInstruction state instr@Instruction {..} =
  case operation of
    Input            -> readUserInput state instr
    Output           -> printTestResult state instr
    JumpIf predicate -> jumpToPointerIf state instr predicate
    Test assert      -> testAssertion state instr assert
    Arithmetic op    -> computeValue state instr op

programWith :: Int -> Int -> [String] -> [String]
programWith = flip replaceAt . pure . show

replaceAt :: Int -> [a] -> [a] -> [a]
replaceAt pos values list =
  let (upper, lower) = splitAt pos list
      suffix = drop (length values) lower
   in upper ++ values ++ suffix

elemAt :: [a] -> Int -> Either String a
elemAt list index
  | 0 <= index && index < length list = Right $ list !! index
  | otherwise = Left $ "Index out of bounds: " ++ show index

paramOf :: ParamMode -> [String] -> Int -> Either String Int
paramOf Position  = \list -> fmap read . elemAt list
paramOf Immediate = flip (const . Right)

runDiagnosticProgram :: [Int] -> [String] -> ProgramOutput
runDiagnosticProgram _ []          = Left "Input is missing!"
runDiagnosticProgram input program = runIntCodeProgram $ ProgramState 0 input program

runIntCodeProgram :: ProgramState -> ProgramOutput
runIntCodeProgram state@ProgramState {..}
  | "99" `elem` program `elemAt` iPointer = Right ["Program halted at code 99."]
  | otherwise = processInstruction $ drop iPointer program
  where
    processInstruction [] = Left "Program reached end of input!"
    processInstruction (instr:args)
      | any (`elem` "34") instr = buildInstruction instr (take 1 args) >>= runInstruction state
      | any (`elem` "56") instr = buildInstruction instr (take 2 args) >>= runInstruction state
      | any (`elem` "1278") instr = buildInstruction instr (take 3 args) >>= runInstruction state
      | otherwise = Left $ "Invalid instruction : " ++ show instr

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

readUserInput :: ProgramState -> Instruction -> ProgramOutput
readUserInput ProgramState {..} (Instruction _ []) = illegalProgramState iPointer program
readUserInput ProgramState {..} (Instruction op ((_, resultP):_)) =
  case input of
    userInput:inputLeft -> runIntCodeProgram $ ProgramState nextP inputLeft (nextProgram userInput)
    _                   -> Left "Missing user input data."
  where
    nextP = nextIPointer op iPointer
    nextProgram userInput = programWith userInput resultP program

printTestResult :: ProgramState -> Instruction -> ProgramOutput
printTestResult state@ProgramState {..} instr =
  case instr of
    Instruction op ((mode, param):_) -> do
      result <- paramOf mode program param
      let output = "Test result at " ++ show iPointer ++ ": " ++ show result
      (output :) <$> runIntCodeProgram state {iPointer = nextIPointer op iPointer}
    _ -> illegalProgramState iPointer program

jumpToPointerIf :: ProgramState -> Instruction -> Predicate -> ProgramOutput
jumpToPointerIf state@ProgramState {..} instr predicate =
  case instr of
    Instruction op ((mode1, param1):(mode2, param2):_) -> do
      arg1 <- paramOf mode1 program param1
      let nextP = nextIPointer op iPointer
      if predicate arg1
        then paramOf mode2 program param2 >>= \newP -> runIntCodeProgram state {iPointer = newP}
        else runIntCodeProgram state {iPointer = nextP}
    _ -> illegalProgramState iPointer program

testAssertion :: ProgramState -> Instruction -> Assertion -> ProgramOutput
testAssertion state@ProgramState {..} instr predicate =
  case instr of
    Instruction op ((mode1, param1):(mode2, param2):(_, resultP):_) -> do
      arg1 <- paramOf mode1 program param1
      arg2 <- paramOf mode2 program param2
      let nextP = nextIPointer op iPointer
      let nextState result = state {iPointer = nextP, program = programWith result resultP program}
      if arg1 `predicate` arg2
        then runIntCodeProgram $ nextState 1
        else runIntCodeProgram $ nextState 0
    _ -> illegalProgramState iPointer program

computeValue :: ProgramState -> Instruction -> Operator -> ProgramOutput
computeValue state@ProgramState {..} instr operator =
  case instr of
    Instruction op ((mode1, param1):(mode2, param2):(_, resultP):_) -> do
      result <- operator <$> paramOf mode1 program param1 <*> paramOf mode2 program param2
      let nextP = nextIPointer op iPointer
      runIntCodeProgram state {iPointer = nextP, program = programWith result resultP program}
    _ -> illegalProgramState iPointer program

illegalProgramState :: Int -> [String] -> ProgramOutput
illegalProgramState iPointer program = Left $ "Illegal program state at: " ++ show (iPointer, program)

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
readInput = parseInput <$> readFile "./resources/input-day5.txt"

showProgramOutput :: ProgramOutput -> String
showProgramOutput (Left err)     = "Error: " ++ err
showProgramOutput (Right output) = unlines output

solutionPart1 :: IO ()
solutionPart1 = readInput >>= putStr . showProgramOutput . runDiagnosticProgram [1]

solutionPart2 :: IO ()
solutionPart2 = readInput >>= putStr . showProgramOutput . runDiagnosticProgram [5]
