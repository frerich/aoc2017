import Data.Bits ((.&.))
import Data.Function (on)
import Prelude hiding (sequenceA)

generator :: Int -> Int -> Int
generator factor input = (input * factor) `rem` 2147483647

run :: (Int -> Int) -> Int -> [Int]
run g = drop 1 . iterate g

sequenceA :: [Int]
sequenceA = run (generator 16807) 883

sequenceB :: [Int]
sequenceB = run (generator 48271) 879

equalPairs :: Int -> [Int] -> [Int] -> Int
equalPairs numPairs seqA seqB = length . filter id . take numPairs $ zipWith ((==) `on` (.&. 0xffff)) seqA seqB

partOne :: Int
partOne = equalPairs 40000000 sequenceA sequenceB

partTwo :: Int
partTwo = equalPairs 5000000 (filter (divides 4) sequenceA) (filter (divides 8) sequenceB)
  where
    divides a b = b `rem` a == 0

main :: IO ()
main = do
    print partOne
    print partTwo
