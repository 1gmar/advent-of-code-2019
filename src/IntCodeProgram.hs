{-# LANGUAGE RecordWildCards #-}

module IntCodeProgram
  ( ProgramState(..)
  , ProgramResult
  , runIntCodeProgram
  , readInputData
  , showResult
  , programState
  , programWithInput
  ) where

import           Data.Char                    (digitToInt, isDigit)
import           Text.ParserCombinators.ReadP (ReadP, char, eof, munch, readP_to_S, sepBy, skipSpaces, (+++))

type ProgramResult = Either String ProgramState

type Parameter = (ParamMode, Int)

type Operator = Int -> Int -> Int

type Assertion = Int -> Int -> Bool

type Predicate = Int -> Bool

data ParamMode
  = Position
  | Immediate
  | Relative

data Operation
  = Arithmetic Operator
  | Input
  | Output
  | JumpIf Predicate
  | Test Assertion
  | UpdateRelBase

data ProgramState =
  ProgramState
    { iPointer     :: Int
    , halted       :: Bool
    , input        :: [Int]
    , output       :: ![Int]
    , program      :: ![String]
    , relativeBase :: Int
    , result       :: Int
    }

data Instruction =
  Instruction
    { operation :: Operation
    , params    :: [Parameter]
    }

toParamMode :: Int -> Either String ParamMode
toParamMode mode =
  case mode of
    0 -> Right Position
    1 -> Right Immediate
    2 -> Right Relative
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
    '9' -> Right UpdateRelBase
    _   -> Left $ "Unknown operation code: " ++ show opCode

nextIPointer :: Operation -> Int -> Int
nextIPointer op =
  case op of
    Arithmetic _  -> (+ 4)
    Input         -> (+ 2)
    Output        -> (+ 2)
    JumpIf _      -> (+ 3)
    Test _        -> (+ 4)
    UpdateRelBase -> (+ 2)

extraMemory :: [String] -> Int -> [String]
extraMemory program index = take (max progSize (index + 1)) (program ++ repeat "0")
  where
    progSize = length program

replaceAt :: Int -> String -> [String] -> [String]
replaceAt index value program =
  let (upper, lower) = splitAt index extendedProg
   in upper ++ value : drop 1 lower
  where
    extendedProg = extraMemory program index

elemAt :: [String] -> Int -> Either String (String, [String])
elemAt program index
  | 0 <= index = Right (extendedProg !! index, extendedProg)
  | otherwise = Left $ "Invalid negative index: " ++ show program
  where
    extendedProg = extraMemory program index

programWith :: Int -> Int -> [String] -> [String]
programWith = flip replaceAt . show

paramOf :: Parameter -> ProgramState -> Either String (Int, [String])
paramOf (mode, param) ProgramState {..} =
  case mode of
    Immediate -> Right (param, program)
    Position  -> program `elemAt` param >>= readResult
    Relative  -> program `elemAt` (relativeBase + param) >>= readResult
  where
    readResult (res, newProg) = Right (read res, newProg)

writeTo :: Parameter -> Int -> Int
writeTo (Relative, resultP) relBase = resultP + relBase
writeTo (_, resultP) _              = resultP

endOfProgram :: [String] -> Int -> Bool
endOfProgram program iPointer = "99" `elem` (fst <$> program `elemAt` iPointer)

returnState :: ProgramState -> Bool -> ProgramResult
returnState state@ProgramState {..} isHalted = Right state {halted = isHalted, output = reverse output}

readInput :: ProgramState -> Instruction -> ProgramResult
readInput ProgramState {..} (Instruction _ []) = illegalProgramState iPointer program
readInput state@ProgramState {..} (Instruction op (param:_)) =
  case input of
    []         -> returnState state False
    value:rest -> runIntCodeProgram $ nextState value rest
  where
    nextP = nextIPointer op iPointer
    resultP = writeTo param relativeBase
    nextState value rest = state {iPointer = nextP, input = rest, program = programWith value resultP program}

writeOutput :: ProgramState -> Instruction -> ProgramResult
writeOutput state@ProgramState {..} instr@Instruction {..} =
  case instr of
    Instruction _ (param:_) -> withParam param >>= runIntCodeProgram . nextState
    _                       -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    withParam param = paramOf param state
    nextState (value, prog) = state {iPointer = nextP, output = value : output, program = prog, result = value}

jumpToPointerIf :: ProgramState -> Instruction -> Predicate -> ProgramResult
jumpToPointerIf state@ProgramState {..} instr@Instruction {..} predicate =
  case instr of
    Instruction _ (param1:param2:_) -> withParam param1 >>= chooseIPointer param2 >>= jumpToAddress
    _                               -> illegalProgramState iPointer program
  where
    jumpToAddress (pointer, prog) = runIntCodeProgram state {iPointer = pointer, program = prog}
    withParam param = paramOf param state
    chooseIPointer param (value, prog)
      | predicate value = paramOf param state {program = prog}
      | otherwise = Right (nextIPointer operation iPointer, prog)

testAssertion :: ProgramState -> Instruction -> Assertion -> ProgramResult
testAssertion state@ProgramState {..} instr@Instruction {..} predicate =
  case instr of
    Instruction _ (p1:p2:p3:_) -> evaluateFor (p1, p2, p3) state nextP test
    _                          -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    test arg1 arg2
      | arg1 `predicate` arg2 = 1
      | otherwise = 0

computeValue :: ProgramState -> Instruction -> Operator -> ProgramResult
computeValue state@ProgramState {..} instr@Instruction {..} operator =
  case instr of
    Instruction _ (p1:p2:p3:_) -> evaluateFor (p1, p2, p3) state nextP operator
    _                          -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer

evaluateFor :: (Parameter, Parameter, Parameter) -> ProgramState -> Int -> Operator -> ProgramResult
evaluateFor (p1, p2, p3) state@ProgramState {..} nextP operator = do
  (arg1, prog1) <- paramOf p1 state
  (arg2, prog2) <- paramOf p2 state {program = prog1}
  let resultP = writeTo p3 relativeBase
  let finalProg = programWith (arg1 `operator` arg2) resultP prog2
  runIntCodeProgram state {iPointer = nextP, program = finalProg}

updateRelativeBase :: ProgramState -> Instruction -> ProgramResult
updateRelativeBase state@ProgramState {..} instr@Instruction {..} =
  case instr of
    Instruction _ (param:_) -> paramOf param state >>= updateBase >>= continueProgram
    _                       -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    updateBase (value, prog) = Right (relativeBase + value, prog)
    continueProgram (base, prog) = runIntCodeProgram state {iPointer = nextP, program = prog, relativeBase = base}

illegalProgramState :: Int -> [String] -> ProgramResult
illegalProgramState iPointer program = Left $ "Illegal program state at: " ++ show (iPointer, program)

runInstruction :: ProgramState -> Instruction -> ProgramResult
runInstruction state instr@Instruction {..} =
  case operation of
    Input            -> readInput state instr
    Output           -> writeOutput state instr
    JumpIf predicate -> jumpToPointerIf state instr predicate
    Test assert      -> testAssertion state instr assert
    Arithmetic op    -> computeValue state instr op
    UpdateRelBase    -> updateRelativeBase state instr

buildInstruction :: String -> [String] -> Either String Instruction
buildInstruction instrCode args =
  case instrCode of
    [opChar]                                      -> instructionWith '0' '0' '0' opChar
    '0':[opChar]                                  -> instructionWith '0' '0' '0' opChar
    paramMode:'0':[opChar]                        -> instructionWith paramMode '0' '0' opChar
    paramMode2:paramMode1:'0':[opChar]            -> instructionWith paramMode1 paramMode2 '0' opChar
    paramMode3:paramMode2:paramMode1:'0':[opChar] -> instructionWith paramMode1 paramMode2 paramMode3 opChar
    _                                             -> Left $ "Unknown instruction: " ++ show instrCode
  where
    paramZipper mode param = (mode, read param)
    instructionWith paramMode1 paramMode2 paramMode3 opChar = do
      mode1 <- toParamMode $ digitToInt paramMode1
      mode2 <- toParamMode $ digitToInt paramMode2
      mode3 <- toParamMode $ digitToInt paramMode3
      op <- toOperation opChar
      let params = zipWith paramZipper [mode1, mode2, mode3] args
      Right $ Instruction op params

runIntCodeProgram :: ProgramState -> ProgramResult
runIntCodeProgram (ProgramState _ _ _ _ [] _ _) = Left "Program is missing!"
runIntCodeProgram state@ProgramState {..}
  | endOfProgram program iPointer = returnState state True
  | otherwise = processInstruction $ drop iPointer program
  where
    processInstruction [] = Left "Program reached end of input!"
    processInstruction (instr:args)
      | any (`elem` "349") instr = buildInstruction instr (take 1 args) >>= runInstruction state
      | any (`elem` "56") instr = buildInstruction instr (take 2 args) >>= runInstruction state
      | any (`elem` "1278") instr = buildInstruction instr (take 3 args) >>= runInstruction state
      | otherwise = Left $ "Invalid instruction : " ++ show instr

inputParser :: ReadP [String]
inputParser = skipSpaces *> commaSeparatedIntegers <* skipSpaces <* eof
  where
    commaSeparatedIntegers = integer `sepBy` char ','
    integer = positive +++ negative
    positive = munch isDigit
    negative = char '-' >>= \sign -> (sign :) <$> positive

parseInput :: String -> [String]
parseInput = concatMap fst . readP_to_S inputParser

readInputData :: String -> IO [String]
readInputData file = parseInput <$> readFile file

showResult :: ProgramResult -> String
showResult progResult =
  case progResult of
    Right state -> show $ result state
    Left err    -> "Error: " ++ err

programState :: [String] -> ProgramState
programState prog = ProgramState 0 False [] [] prog 0 0

programWithInput :: [String] -> [Int] -> ProgramState
programWithInput prog inputData = (programState prog) {input = inputData}
