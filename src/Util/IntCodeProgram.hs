module Util.IntCodeProgram
  ( Program (input, result, halted, iPointer),
    ProgramResult,
    newProgram,
    programWithInput,
    programWithOutput,
    parseIntCode,
    programMemory,
    outputList,
    runIntCodeProgram,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.ST (runST)
import Control.Monad.State (StateT, execStateT, get, gets, modify')
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..), unfoldr)
import qualified Data.List.NonEmpty as NE (init)
import Data.Tuple (swap)
import Data.Vector.Unboxed
  ( Vector,
    empty,
    fromList,
    modify,
    slice,
    toList,
    unsafeFreeze,
    unsafeThaw,
    (!),
  )
import qualified Data.Vector.Unboxed as V (length, null)
import Data.Vector.Unboxed.Mutable (grow, write)
import Util.ParseUtils

type ProgramState = StateT Program (Either String)

type ProgramResult = Either String Program

type Parameter = (Int, ParamMode)

type Operator = Int -> Int -> Int

type Predicate = Int -> Bool

type IntV = Vector Int

type ArgCount = Int

type EndRoutine = ProgramState ()

data ParamMode
  = Position
  | Immediate
  | Relative
  deriving (Show)

data Operation
  = Add
  | Mult
  | Input
  | Output
  | JumpNEZ
  | JumpEQZ
  | IfLess
  | IfEqual
  | UpdateRelBase
  deriving (Eq, Enum, Bounded, Show)

data Instruction = Instruction !EndRoutine ![Parameter]

data Program = Program
  { iPointer :: !Int,
    halted :: !Bool,
    input :: ![Int],
    output :: !IntV,
    memory :: !IntV,
    relativeBase :: !Int,
    result :: !Int
  }

opToEnum :: Int -> Operation
opToEnum = toEnum . subtract 1

setMemory :: IntV -> Program -> Program
setMemory list prog = prog {memory = list}

instructionRegistry :: [(ArgCount, [Operation])]
instructionRegistry =
  [ (1, [Input, Output, UpdateRelBase]),
    (2, [JumpNEZ, JumpEQZ]),
    (3, [Add, Mult, IfLess, IfEqual])
  ]

tokenize :: Int -> NonEmpty Int
tokenize 0 = 0 :| []
tokenize intCode = unfoldr maybeDiv intCode
  where
    maybeDiv instr
      | instr > 0 = Just <$> swap (divMod instr 10)
      | otherwise = (instr, Nothing)

illegalState :: Show a => String -> a -> ProgramState b
illegalState message instr =
  gets iPointer >>= \i -> throwError $ concat [message, show instr, " at index: ", show i]

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

valueOf :: Parameter -> ProgramState Int
valueOf (param, mode) =
  case mode of
    Immediate -> return param
    Position -> elemAt param
    Relative -> gets relativeBase >>= elemAt . (+ param)

indexOf :: Parameter -> ProgramState Int
indexOf (resultP, mode) =
  case mode of
    Relative -> gets ((+ resultP) . relativeBase)
    _ -> return resultP

yieldState :: Bool -> ProgramState ()
yieldState isHalted = modify' setHalted
  where
    setHalted program = program {halted = isHalted}

readInput :: Instruction -> ProgramState ()
readInput (Instruction end ~[parameter]) =
  gets input
    >>= ( \case
            [] -> yieldState False
            value : rest -> processInput value rest
        )
  where
    setInput value program = program {input = value}
    processInput value rest = do
      resultP <- indexOf parameter
      replaceAt resultP value
      modify' (setInput rest)
      end

writeOutput :: Instruction -> ProgramState ()
writeOutput (Instruction end ~[parameter]) = do
  value <- valueOf parameter
  modify' (appendOutput value)
  modify' (setResult value)
  end
  where
    setResult value program = program {result = value}
    appendOutput value program@Program {..} = program {output = output `mutSnoc` value}

jumpToPointerIf :: Predicate -> Instruction -> ProgramState ()
jumpToPointerIf predicate (Instruction end ~[param1, param2]) = valueOf param1 >>= selectEndRoutine
  where
    setIPointer value program = program {iPointer = value}
    selectEndRoutine value
      | predicate value = valueOf param2 >>= modify' . setIPointer >> programStep
      | otherwise = end

computeValue :: Operator -> Instruction -> ProgramState ()
computeValue operator (Instruction end ~[param1, param2, param3]) = do
  value1 <- valueOf param1
  value2 <- valueOf param2
  resultP <- indexOf param3
  replaceAt resultP (value1 `operator` value2)
  end

updateRelativeBase :: Instruction -> ProgramState ()
updateRelativeBase (Instruction end ~[parameter]) = do
  value <- valueOf parameter
  modify' (updateBase value)
  end
  where
    updateBase value program@Program {..} = program {relativeBase = relativeBase + value}

runInstruction :: (Operation, Instruction) -> ProgramState ()
runInstruction (operation, instruction) =
  case operation of
    Input -> readInput instruction
    Output -> writeOutput instruction
    UpdateRelBase -> updateRelativeBase instruction
    JumpNEZ -> jumpToPointerIf (/= 0) instruction
    JumpEQZ -> jumpToPointerIf (== 0) instruction
    IfLess -> computeValue (assert (<)) instruction
    IfEqual -> computeValue (assert (==)) instruction
    Add -> computeValue (+) instruction
    Mult -> computeValue (*) instruction
  where
    assert cmp lhs rhs
      | lhs `cmp` rhs = 1
      | otherwise = 0

buildInstruction :: [Int] -> [Int] -> ProgramState (Operation, Instruction)
buildInstruction instrTokens args =
  case instrTokens of
    [opCode] -> instructionWith opCode zeros
    [opCode, 0] -> instructionWith opCode zeros
    [opCode, 0, pmCode] -> instructionWith opCode (pmCode : zeros)
    [opCode, 0, pmCode1, pmCode2] -> instructionWith opCode (pmCode1 : pmCode2 : zeros)
    [opCode, 0, pmCode1, pmCode2, pmCode3] -> instructionWith opCode (pmCode1 : pmCode2 : pmCode3 : zeros)
    _ -> illegalState "Invalid instruction format: " instrTokens
  where
    zeros = replicate (length args) 0
    endRoutineFor argsLen = moveIPointerBy (argsLen + 1) >> programStep
    instructionWith opCode pmCodes = do
      paramModes <- mapM toParamMode pmCodes
      let endR = endRoutineFor $ length args
      return (opToEnum opCode, Instruction endR (args `zip` paramModes))

processInstruction :: Int -> ProgramState ()
processInstruction instr =
  case fst <$> maybeInstrPair of
    Just argsLen -> get >>= buildInstruction (NE.init tokens) . takeArgs argsLen >>= runInstruction
    Nothing -> illegalState "Illegal instruction: " instr
  where
    tokens@(opCode :| _) = tokenize instr
    maybeInstrPair = find ((opToEnum opCode `elem`) . snd) instructionRegistry
    takeArgs argsLen Program {..} = toList $ slice (iPointer + 1) argsLen memory

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
