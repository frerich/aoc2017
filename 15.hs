import Data.Bits ((.&.))
import Data.Function (on)

generator :: Int -> Int -> Int
generator factor input = (input * factor) `rem` 2147483647

equalPairs :: Int -> (Int -> Bool) -> (Int -> Bool) -> Int
equalPairs numPairs predA predB = length . filter id . take numPairs $ zipWith ((==) `on` (.&. 0xffff)) (filter predA seqA) (filter predB seqB)
  where
    seqA = iterate (generator 16807) 883
    seqB = iterate (generator 48271) 879

partOne :: Int
partOne = equalPairs 40000000 (const True) (const True)

partTwo :: Int
partTwo = equalPairs 5000000 (divides 4) (divides 8)
  where
    divides a b = b `rem` a == 0

main :: IO ()
main = do
    print partOne
    print partTwo
