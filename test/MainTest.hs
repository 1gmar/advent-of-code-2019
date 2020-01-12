import qualified Day1Test as Day1
import qualified Day2Test as Day2
import qualified Day3Test as Day3
import qualified Day4Test as Day4

main :: IO ()
main = putStrLn " Running AoC tests:" >> Day1.runTests >> Day2.runTests >> Day3.runTests >> Day4.runTests
