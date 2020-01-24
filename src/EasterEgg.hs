module EasterEgg
  ( runEasterEgg
  ) where

import           Data.Char      (chr)
import           IntCodeProgram

getEasterEgg :: [Int] -> Either String String
getEasterEgg = fmap (map chr . output) . runIntCodeProgram . programState

writeEgg :: Either String String -> IO ()
writeEgg (Left err)  = putStrLn err
writeEgg (Right egg) = writeFile "./resources/output/easter-egg.txt" egg

runEasterEgg :: IO ()
runEasterEgg = readFile "./resources/input/easter-egg.txt" >>= writeEgg . getEasterEgg . parseIntCode
