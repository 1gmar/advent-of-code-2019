import qualified Day1Spec as Day1
import qualified Day2Spec as Day2

main :: IO ()
main = putStrLn " Running AoC tests:" >> Day1.runTests >> Day2.runTests
