import qualified Day1Test as Day1
import qualified Day2Test as Day2
import qualified Day3Test as Day3
import qualified Day4Test as Day4
import qualified Day5Test as Day5
import qualified Day6Test as Day6
import qualified Day7Test as Day7
import qualified Day8Test as Day8
import qualified Day9Test as Day9

main :: IO ()
main =
  putStrLn " Running AoC tests:" >>
  sequence_ [Day1.test, Day2.test, Day3.test, Day4.test, Day5.test, Day6.test, Day7.test, Day8.test, Day9.test]
