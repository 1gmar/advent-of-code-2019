import qualified Day1Test as Day1
import qualified Day2Test as Day2
import qualified Day3Test as Day3

main :: IO ()
main = putStrLn " Running AoC tests:" >> Day1.runTests >> Day2.runTests >> Day3.runTests
