{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Util.IntCodeProgramV2
  ( Program(input, result, halted, iPointer)
  , ProgramResult
  , programState
  , programWithInput
  , programWithOutput
  , parseIntCode
  , programList
  , outputList
  , runIntCodeProgram
  ) where

import           Control.Monad               (when)
import           Control.Monad.ST            (runST)
import           Control.Monad.State         (StateT, execStateT, get, gets, modify')
import           Control.Monad.Trans.Class   (lift)
import           Data.List                   (find, unfoldr)
import           Data.Tuple                  (swap)
import           Data.Vector.Unboxed         (Vector, empty, fromList, modify, slice, toList, unsafeFreeze, unsafeThaw,
                                              (!))
import qualified Data.Vector.Unboxed         as V (length, null)
import           Data.Vector.Unboxed.Mutable (grow, write)
import           Util.ParseUtils

-- TODO MonadFail instance

type ProgramState = StateT Program (Either String)
type ProgramResult = Either String Program
type Parameter = (Int, ParamMode)
type Operator = Int -> Int -> Int
type Predicate = Int -> Bool
type IntV = Vector Int

data ParamMode
  = Position
  | Immediate
  | Relative
  deriving Show

data Operation
  = Evaluate Operator
  | Input
  | Output
  | JumpIf Predicate
  | UpdateRelBase

data Instruction
  = Instruction
      { operation :: !Operation
      , params    :: ![Parameter]
      }
  deriving Show

data Program
  = Program
      { iPointer     :: !Int
      , halted       :: !Bool
      , input        :: ![Int]
      , output       :: !IntV
      , program      :: !IntV
      , relativeBase :: !Int
      , result       :: !Int
      }

instance Show Operation where
  show = \case
    Evaluate _    -> "Evaluate"
    Input         -> "Input"
    Output        -> "Output"
    JumpIf _      -> "JumpIf"
    UpdateRelBase -> "UpdateRelBase"

setProgram :: IntV -> Program -> Program
setProgram list prog = prog {program = list}

argLengthToInstrCode :: [(Int, [Int])]
argLengthToInstrCode = [(1, [3, 4, 9]), (2, [5, 6]), (3, [1, 2, 7, 8])]

tokenize :: Int -> [Int]
tokenize 0 = [0]
tokenize intCode = unfoldr maybeDiv intCode
  where
    maybeDiv instr
      | instr > 0 = Just $ swap $ divMod instr 10
      | otherwise = Nothing

illegalState :: Show a => String -> a -> ProgramState b
illegalState message instr = gets iPointer >>= \i ->
  lift $ Left $ concat [message, show instr, " at index: ", show i]

toParamMode :: Int -> ProgramState ParamMode
toParamMode mode =
  case mode of
    0 -> return Position
    1 -> return Immediate
    2 -> return Relative
    _ -> illegalState "Invalid parameter mode: " mode

toOperation :: Int -> Operation
toOperation = \case
  1  -> Evaluate (+)
  2  -> Evaluate (*)
  3  -> Input
  4  -> Output
  5  -> JumpIf (/= 0)
  6  -> JumpIf (== 0)
  7  -> Evaluate (assert (<))
  8  -> Evaluate (assert (==))
  ~9 -> UpdateRelBase
  where
    assert condition arg1 arg2
      | arg1 `condition` arg2 = 1
      | otherwise = 0

updateIPointer :: Operation -> ProgramState ()
updateIPointer op = modify' setPointer
  where
    setPointer state@Program {..} = state {iPointer = moveIPointer op iPointer}

moveIPointer :: Operation -> (Int -> Int)
moveIPointer = \case
  Evaluate _    -> (+ 4)
  Input         -> (+ 2)
  Output        -> (+ 2)
  JumpIf _      -> (+ 3)
  UpdateRelBase -> (+ 2)

growList :: IntV -> Int -> IntV
growList list by
  | by <= 0 = list
  | otherwise = runST $ do
    mutList <- unsafeThaw list
    grow mutList by >>= unsafeFreeze

mutSnoc :: IntV -> Int -> IntV
mutSnoc list value = modify writeValue newList
  where
    newList = growList list 1
    index = V.length list
    writeValue vector = write vector index value

maybeGrow :: Int -> ProgramState ()
maybeGrow index
  | 0 <= index = extendProgram
  | otherwise = illegalState "Invalid negative instruction pointer: " index
  where
    extendProgram = do
      list <- gets program
      let progSize = V.length list
      when (progSize <= index) $ modify' (setProgram $ growList list (index - progSize + 1))

elemAt :: Int -> ProgramState Int
elemAt index = maybeGrow index >> gets ((! index) . program)

replaceAt :: Int -> Int -> ProgramState ()
replaceAt index value = maybeGrow index >> modify' updateProg
  where
    writeValue vector = write vector index value
    updateProg state@Program {..} = modify writeValue program `setProgram` state

paramOf :: Parameter -> ProgramState Int
paramOf (param, mode) =
  case mode of
    Immediate -> return param
    Position  -> elemAt param
    Relative  -> gets relativeBase >>= elemAt . (+ param)

writeTo :: Parameter -> ProgramState Int
writeTo (resultP, mode) =
  case mode of
    Relative -> gets ((+ resultP) . relativeBase)
    _        -> return resultP

yieldState :: Bool -> ProgramState ()
yieldState isHalted = modify' setHalted
  where
    setHalted state = state {halted = isHalted}

readInput :: Instruction -> ProgramState ()
readInput ~(Instruction operation [parameter]) = gets input >>= (\case
  [] -> yieldState False
  value:rest -> processInput value rest)
  where
    setInput value state = state {input = value}
    processInput value rest = do
      resultP <- writeTo parameter
      replaceAt resultP value
      modify' (setInput rest)
      updateIPointer operation
      programStep

writeOutput :: Instruction -> ProgramState ()
writeOutput ~(Instruction operation [parameter]) = do
  value <- paramOf parameter
  modify' (appendOutput value)
  modify' (setResult value)
  updateIPointer operation
  programStep
  where
    setResult value state = state {result = value}
    appendOutput value state@Program {..} = state {output = output `mutSnoc` value}

jumpToPointerIf :: Predicate -> Instruction -> ProgramState ()
jumpToPointerIf predicate ~(Instruction operation [param1, param2]) = do
  value <- paramOf param1
  resultP <- chooseNextIPointer value
  modify' (setIPointer resultP)
  programStep
  where
    setIPointer value state = state {iPointer = value}
    chooseNextIPointer value
      | predicate value = paramOf param2
      | otherwise = gets (moveIPointer operation . iPointer)

computeValue :: Operator -> Instruction -> ProgramState ()
computeValue operator ~(Instruction operation [param1, param2, param3]) = do
  value1 <- paramOf param1
  value2 <- paramOf param2
  resultP <- writeTo param3
  replaceAt resultP (value1 `operator` value2)
  updateIPointer operation
  programStep

updateRelativeBase :: Instruction -> ProgramState ()
updateRelativeBase ~(Instruction operation [parameter]) = do
  value <- paramOf parameter
  modify' (updateBase value)
  updateIPointer operation
  programStep
  where
    updateBase value state@Program {..} = state {relativeBase = relativeBase + value}

runInstruction :: Instruction -> ProgramState ()
runInstruction instruction@Instruction {..} =
  case operation of
    Input             -> readInput instruction
    Output            -> writeOutput instruction
    JumpIf predicate  -> jumpToPointerIf predicate instruction
    Evaluate operator -> computeValue operator instruction
    UpdateRelBase     -> updateRelativeBase instruction

buildInstruction :: [Int] -> [Int] -> ProgramState Instruction
buildInstruction instrTokens args =
  case instrTokens of
    [opCode]                               -> instructionWith opCode zeros
    [opCode, 0]                            -> instructionWith opCode zeros
    [opCode, 0, pmCode]                    -> instructionWith opCode (pmCode : zeros)
    [opCode, 0, pmCode1, pmCode2]          -> instructionWith opCode (pmCode1 : pmCode2 : zeros)
    [opCode, 0, pmCode1, pmCode2, pmCode3] -> instructionWith opCode (pmCode1 : pmCode2 : pmCode3 : zeros)
    _                                      -> illegalState "Invalid instruction format: " instrTokens
  where
    zeros = replicate (length args) 0
    instructionWith opCode pmCodes = Instruction (toOperation opCode) . zip args <$> mapM toParamMode pmCodes

processInstruction :: Int -> ProgramState ()
processInstruction instr =
  case fst <$> instrCodeMatch of
    Just argsLen -> get >>= buildInstruction tokens . args argsLen >>= runInstruction
    Nothing      -> illegalState "Illegal instruction: " instr
  where
    args argsLen Program {..} = toList $ slice (iPointer + 1) argsLen program
    tokens@(opCode:_) = tokenize instr
    instrCodeMatch = find ((opCode `elem`) . snd) argLengthToInstrCode

programStep :: ProgramState ()
programStep = do
  prog <- gets program
  if V.null prog
    then illegalState "Program is null: " prog
    else gets iPointer >>= elemAt >>= stepIn
  where
    stepIn instr
      | instr == 99 = yieldState True
      | otherwise = processInstruction instr

parseIntCode :: String -> [Int]
parseIntCode = parseInput inputParser
  where
    inputParser = trimSpacesEOF $ integer `sepBy` char ','

programState :: [Int] -> Program
programState prog = Program 0 False [] empty (fromList prog) 0 0

programList :: Program -> [Int]
programList = toList . program

outputList :: Program -> [Int]
outputList = toList . output

runIntCodeProgram :: Program -> ProgramResult
runIntCodeProgram = execStateT programStep

programWithInput :: [Int] -> [Int] -> Program
programWithInput prog inputData = (programState prog) {input = inputData}

programWithOutput :: Program -> [Int] -> Program
programWithOutput progState outData = progState {output = fromList outData}
