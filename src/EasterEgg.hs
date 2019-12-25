module EasterEgg
  ( runEasterEgg
  ) where

import           Data.Char      (chr)
import           IntCodeProgram

getEasterEgg :: [String] -> Either String String
getEasterEgg prog = collectEgg [] $ runIntCodeProgram $ ProgramState 0 [] prog 0 0 True False
  where
    collectEgg egg progResult = do
      state <- progResult
      if halted state
        then Right egg
        else collectEgg (egg ++ [chr $ result state]) $ runIntCodeProgram state

writeEgg :: Either String String -> IO ()
writeEgg (Left err)  = putStrLn err
writeEgg (Right egg) = writeFile "./out/easter-egg.txt" egg

runEasterEgg :: IO ()
runEasterEgg = readInputData "./resources/easter-egg.txt" >>= writeEgg . getEasterEgg
