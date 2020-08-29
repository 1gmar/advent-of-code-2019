{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Util.IntCodeProgram
  ( Program(input, result, halted, iPointer)
  , ProgramResult
  , newProgram
  , programWithInput
  , programWithOutput
  , parseIntCode
  , programMemory
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
type EndRoutine = ProgramState ()

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
      { operation  :: !Operation
      , endRoutine :: !EndRoutine
      , parameters :: ![Parameter]
      }

data Program
  = Program
      { iPointer     :: !Int
      , halted       :: !Bool
      , input        :: ![Int]
      , output       :: !IntV
      , memory       :: !IntV
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
    assert cmp arg1 arg2
      | arg1 `cmp` arg2 = 1
      | otherwise = 0

setMemory :: IntV -> Program -> Program
setMemory list prog = prog {memory = list}

instructionRegistry :: [(Int, [Int])]
instructionRegistry = [(1, [3, 4, 9]), (2, [5, 6]), (3, [1, 2, 7, 8])]

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

moveIPointerBy :: Int -> ProgramState ()
moveIPointerBy value = modify' updateIPointer
  where
    updateIPointer program@Program {..} = program {iPointer = iPointer + value}

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
      list <- gets memory
      let progSize = V.length list
      when (progSize <= index) $ modify' (setMemory $ growList list (index - progSize + 1))

elemAt :: Int -> ProgramState Int
elemAt index = maybeGrow index >> gets ((! index) . memory)

replaceAt :: Int -> Int -> ProgramState ()
replaceAt index value = maybeGrow index >> modify' updateProg
  where
    writeValue vector = write vector index value
    updateProg program@Program {..} = modify writeValue memory `setMemory` program

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
    setHalted program = program {halted = isHalted}

readInput :: EndRoutine -> [Parameter] -> ProgramState ()
readInput end ~[parameter] = gets input >>= (\case
  [] -> yieldState False
  value:rest -> processInput value rest)
  where
    setInput value program = program {input = value}
    processInput value rest = do
      resultP <- writeTo parameter
      replaceAt resultP value
      modify' (setInput rest)
      end

writeOutput :: EndRoutine -> [Parameter] -> ProgramState ()
writeOutput end ~[parameter] = do
  value <- paramOf parameter
  modify' (appendOutput value)
  modify' (setResult value)
  end
  where
    setResult value program = program {result = value}
    appendOutput value program@Program {..} = program {output = output `mutSnoc` value}

jumpToPointerIf :: Predicate -> EndRoutine -> [Parameter] -> ProgramState ()
jumpToPointerIf predicate end ~[param1, param2] = paramOf param1 >>= selectEndRoutine
  where
    setIPointer value program = program {iPointer = value}
    selectEndRoutine value
      | predicate value = paramOf param2 >>= modify' . setIPointer >> programStep
      | otherwise = end

computeValue :: Operator -> EndRoutine -> [Parameter] -> ProgramState ()
computeValue operator end ~[param1, param2, param3] = do
  value1 <- paramOf param1
  value2 <- paramOf param2
  resultP <- writeTo param3
  replaceAt resultP (value1 `operator` value2)
  end

updateRelativeBase :: EndRoutine -> [Parameter] -> ProgramState ()
updateRelativeBase end ~[parameter] = do
  value <- paramOf parameter
  modify' (updateBase value)
  end
  where
    updateBase value program@Program {..} = program {relativeBase = relativeBase + value}

runInstruction :: Instruction -> ProgramState ()
runInstruction Instruction {..} =
  case operation of
    Input             -> readInput endRoutine parameters
    Output            -> writeOutput endRoutine parameters
    JumpIf predicate  -> jumpToPointerIf predicate endRoutine parameters
    Evaluate operator -> computeValue operator endRoutine parameters
    UpdateRelBase     -> updateRelativeBase endRoutine parameters

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
    endRoutineFor argsLen = moveIPointerBy (argsLen + 1) >> programStep
    instructionWith opCode pmCodes = do
      params <- zip args <$> mapM toParamMode pmCodes
      let endR = endRoutineFor $ length args
      return $ Instruction (toOperation opCode) endR params

processInstruction :: Int -> ProgramState ()
processInstruction instr =
  case fst <$> instrCodeMatch of
    Just argsLen -> get >>= buildInstruction tokens . args argsLen >>= runInstruction
    Nothing      -> illegalState "Illegal instruction: " instr
  where
    args argsLen Program {..} = toList $ slice (iPointer + 1) argsLen memory
    tokens@(opCode:_) = tokenize instr
    instrCodeMatch = find ((opCode `elem`) . snd) instructionRegistry

programStep :: ProgramState ()
programStep = gets memory >>= stepInUnlessNull
  where
    stepInUnlessNull memory
      | V.null memory = illegalState "Program memory is null: " memory
      | otherwise = gets iPointer >>= elemAt >>= stepIn
    stepIn instr
      | instr == 99 = yieldState True
      | otherwise = processInstruction instr

parseIntCode :: String -> [Int]
parseIntCode = parseInput inputParser
  where
    inputParser = trimSpacesEOF $ integer `sepBy` char ','

newProgram :: [Int] -> Program
newProgram memory = Program 0 False [] empty (fromList memory) 0 0

programMemory :: Program -> [Int]
programMemory = toList . memory

outputList :: Program -> [Int]
outputList = toList . output

runIntCodeProgram :: Program -> ProgramResult
runIntCodeProgram = execStateT programStep

programWithInput :: Program -> [Int] -> Program
programWithInput program inputData = program {input = inputData}

programWithOutput :: Program -> [Int] -> Program
programWithOutput program outData = program {output = fromList outData}
