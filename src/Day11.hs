{-# LANGUAGE RecordWildCards #-}

module Day11
  ( solutionPart1
  , solutionPart2
  ) where

import           Data.Char                    (digitToInt, isDigit)
import           Data.List                    (find, groupBy, maximum, minimum, sortOn, unionBy, (\\))
import           Text.ParserCombinators.ReadP (ReadP, char, eof, munch, readP_to_S, sepBy, skipSpaces, (+++))

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
    , input        :: [Int]
    , program      :: [String]
    , result       :: Int
    , relativeBase :: Int
    , halted       :: Bool
    }

data Instruction =
  Instruction
    { operation :: Operation
    , params    :: [Parameter]
    }

data Direction
  = LEFT
  | RIGHT
  | UP
  | DOWN
  deriving (Enum)

data Color
  = Black
  | White
  deriving (Enum, Eq)

data Panel =
  Panel
    { position :: Position
    , color    :: Color
    }

instance Eq Panel where
  (Panel pos1 _) == (Panel pos2 _) = pos1 == pos2

data Robot =
  Robot
    { panel     :: Panel
    , direction :: Direction
    , software  :: ProgramState
    , grid      :: PanelGrid
    }

type ProgramOutput = Either String ProgramState

type Parameter = (ParamMode, Int)

type Operator = Int -> Int -> Int

type Assertion = Int -> Int -> Bool

type Predicate = Int -> Bool

type Position = (Int, Int)

type PanelGrid = [Panel]

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

readInput :: ProgramState -> Instruction -> ProgramOutput
readInput ProgramState {..} (Instruction _ []) = illegalProgramState iPointer program
readInput state@ProgramState {..} (Instruction op (param:_)) =
  case input of
    value:rest -> runIntCodeProgram $ nextState value rest
    _          -> Left "Missing program input data."
  where
    nextP = nextIPointer op iPointer
    resultP = writeTo param relativeBase
    nextState value rest = state {iPointer = nextP, input = rest, program = programWith value resultP program}

writeOutput :: ProgramState -> Instruction -> ProgramOutput
writeOutput state@ProgramState {..} instr@Instruction {..} =
  case instr of
    Instruction _ (param:_) -> withParam param >>= Right . nextState
    _                       -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    withParam param = paramOf param state
    nextState (value, prog) = state {iPointer = nextP, result = value, program = prog, halted = endOfProgram prog nextP}

jumpToPointerIf :: ProgramState -> Instruction -> Predicate -> ProgramOutput
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

testAssertion :: ProgramState -> Instruction -> Assertion -> ProgramOutput
testAssertion state@ProgramState {..} instr@Instruction {..} predicate =
  case instr of
    Instruction _ (p1:p2:p3:_) -> evaluateFor (p1, p2, p3) state nextP test
    _                          -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    test arg1 arg2
      | arg1 `predicate` arg2 = 1
      | otherwise = 0

computeValue :: ProgramState -> Instruction -> Operator -> ProgramOutput
computeValue state@ProgramState {..} instr@Instruction {..} operator =
  case instr of
    Instruction _ (p1:p2:p3:_) -> evaluateFor (p1, p2, p3) state nextP operator
    _                          -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer

evaluateFor :: (Parameter, Parameter, Parameter) -> ProgramState -> Int -> Operator -> ProgramOutput
evaluateFor (p1, p2, p3) state@ProgramState {..} nextP operator = do
  (arg1, prog1) <- paramOf p1 state
  (arg2, prog2) <- paramOf p2 state {program = prog1}
  let resultP = writeTo p3 relativeBase
  let finalProg = programWith (arg1 `operator` arg2) resultP prog2
  runIntCodeProgram state {iPointer = nextP, program = finalProg}

updateRelativeBase :: ProgramState -> Instruction -> ProgramOutput
updateRelativeBase state@ProgramState {..} instr@Instruction {..} =
  case instr of
    Instruction _ (param:_) -> paramOf param state >>= updateBase >>= continueProgram
    _                       -> illegalProgramState iPointer program
  where
    nextP = nextIPointer operation iPointer
    updateBase (value, prog) = Right (relativeBase + value, prog)
    continueProgram (base, prog) = runIntCodeProgram state {iPointer = nextP, program = prog, relativeBase = base}

illegalProgramState :: Int -> [String] -> ProgramOutput
illegalProgramState iPointer program = Left $ "Illegal program state at: " ++ show (iPointer, program)

runInstruction :: ProgramState -> Instruction -> ProgramOutput
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

runIntCodeProgram :: ProgramState -> ProgramOutput
runIntCodeProgram state@ProgramState {..}
  | endOfProgram program iPointer = Right state {halted = True}
  | otherwise = processInstruction $ drop iPointer program
  where
    processInstruction [] = Left "Program reached end of input!"
    processInstruction (instr:args)
      | any (`elem` "349") instr = buildInstruction instr (take 1 args) >>= runInstruction state
      | any (`elem` "56") instr = buildInstruction instr (take 2 args) >>= runInstruction state
      | any (`elem` "1278") instr = buildInstruction instr (take 3 args) >>= runInstruction state
      | otherwise = Left $ "Invalid instruction : " ++ show instr

currentColor :: Robot -> Color
currentColor Robot {..} =
  case find (== panel) grid of
    Just (Panel _ color) -> color
    Nothing              -> Black

paintCurrentPanel :: Robot -> Color -> Robot
paintCurrentPanel robot@Robot {..} newColor =
  case find (== panel) grid of
    Just pan -> robot {grid = (pan {color = newColor}) : (grid \\ [pan])}
    Nothing  -> robot {grid = Panel (position panel) newColor : grid}

rotateRobot :: Robot -> Direction -> Robot
rotateRobot robot@Robot {..} newDirection =
  case (newDirection, direction) of
    (LEFT, DOWN)  -> robot {direction = RIGHT}
    (LEFT, RIGHT) -> robot {direction = UP}
    (LEFT, UP)    -> robot {direction = LEFT}
    (RIGHT, DOWN) -> robot {direction = LEFT}
    (RIGHT, LEFT) -> robot {direction = UP}
    (RIGHT, UP)   -> robot {direction = RIGHT}
    _             -> robot {direction = DOWN}

moveRobot :: Robot -> Robot
moveRobot robot@(Robot pan@(Panel (x, y) _) dir _ _) =
  case dir of
    LEFT  -> robot {panel = pan {position = (x - 1, y)}}
    RIGHT -> robot {panel = pan {position = (x + 1, y)}}
    UP    -> robot {panel = pan {position = (x, y - 1)}}
    DOWN  -> robot {panel = pan {position = (x, y + 1)}}

paintShip :: Robot -> Either String Robot
paintShip robot@Robot {..} =
  case robot of
    Robot _ _ (ProgramState _ _ _ _ _ True) _ -> Right robot
    Robot _ _ state _ -> do
      let color = fromEnum $ currentColor robot
      stateWithColor <- runIntCodeProgram state {input = [color, color]}
      stateWithDirection <- runIntCodeProgram stateWithColor
      let brushRobot = paintCurrentPanel robot $ toEnum (result stateWithColor)
      let rotatedRobot = rotateRobot brushRobot $ toEnum (result stateWithDirection)
      paintShip $ (moveRobot rotatedRobot) {software = stateWithDirection}

runPaintingRobot :: Color -> [String] -> Either String Robot
runPaintingRobot startColor prog = paintShip $ Robot pan UP soft [pan]
  where
    soft = ProgramState 0 [] prog 0 0 False
    pan = Panel (0, 0) startColor

countPaintedPanels :: Robot -> Int
countPaintedPanels Robot {..} = length grid

showPanel :: Panel -> Char
showPanel Panel {..} =
  case color of
    Black -> '⬜'
    White -> '⬛'

fillGridLine :: (Int, Int) -> PanelGrid -> PanelGrid
fillGridLine (lower, upper) ~line@(Panel (_, y) _:_) = sortOn (fst . position) fullLine
  where
    fullLine = unionBy xCoordinate line fillerLine
    fillerLine = map (`Panel` Black) $ [lower .. upper] `zip` repeat y
    xCoordinate (Panel (x1, _) _) (Panel (x2, _) _) = x1 == x2

showRegistrationNumber :: Robot -> String
showRegistrationNumber Robot {..} = unlines $ foldr showPanels [] fullGridLines
  where
    xs = map (fst . position) grid
    [minX, maxX] = map (\f -> f xs) [minimum, maximum]
    gridLines = groupBy yCoordinate $ sortOn (snd . position) grid
    fullGridLines = fillGridLine (minX, maxX) <$> gridLines
    yCoordinate (Panel (_, y1) _) (Panel (_, y2) _) = y1 == y2
    showPanels gridRow rows = map showPanel gridRow : rows

writeResult :: Either String String -> IO ()
writeResult (Left err)     = putStrLn err
writeResult (Right panels) = writeFile "./out/output-day11.txt" panels

inputParser :: ReadP [String]
inputParser = skipSpaces *> commaSeparatedIntegers <* skipSpaces <* eof
  where
    commaSeparatedIntegers = integer `sepBy` char ','
    integer = positive +++ negative
    positive = munch isDigit
    negative = char '-' >>= \sign -> (sign :) <$> positive

parseInput :: String -> [String]
parseInput = concatMap fst . readP_to_S inputParser

readInputData :: IO [String]
readInputData = parseInput <$> readFile "./resources/input-day11.txt"

showCount :: Either String Int -> String
showCount (Left err)  = "Error: " ++ err
showCount (Right res) = show res

solutionPart1 :: IO String
solutionPart1 = showCount . fmap countPaintedPanels . runPaintingRobot Black <$> readInputData

solutionPart2 :: IO ()
solutionPart2 = readInputData >>= writeResult . fmap showRegistrationNumber . runPaintingRobot White
