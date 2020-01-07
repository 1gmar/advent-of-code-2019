module EasterEgg
  ( runEasterEgg
  ) where

import           Data.Char      (chr)
import           IntCodeProgram

getEasterEgg :: [Int] -> Either String String
getEasterEgg = fmap (map chr . output) . runIntCodeProgram . programState

writeEgg :: Either String String -> IO ()
writeEgg (Left err)  = putStrLn err
writeEgg (Right egg) = writeFile "./out/easter-egg.txt" egg

runEasterEgg :: IO ()
runEasterEgg = readInputData "./resources/easter-egg.txt" >>= writeEgg . getEasterEgg
