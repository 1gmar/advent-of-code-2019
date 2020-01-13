import qualified Day1Test as Day1
import qualified Day2Test as Day2
import qualified Day3Test as Day3
import qualified Day4Test as Day4
import qualified Day5Test as Day5

main :: IO ()
main = putStrLn " Running AoC tests:" >> sequence_ [Day1.test, Day2.test, Day3.test, Day4.test, Day5.test]
