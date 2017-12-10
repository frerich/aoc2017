import Data.Bits (xor, shiftR, (.&.))
import Data.Char (ord, intToDigit)
import Data.List.Split (splitOn, chunksOf)
import Prelude hiding (round)

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

round :: [Int] -> (Int, Int, [Int]) -> (Int, Int, [Int])
round = flip (foldl go)
  where
    go (pos, skipSize, xs) len = (pos + len + skipSize, skipSize + 1, rotate (len + skipSize) (step len xs))

    step :: Int -> [a] -> [a]
    step len xs = let (as, bs) = splitAt len xs in reverse as ++ bs

hash :: Int -> [Int] -> [Int]
hash rounds numbers = rotate (length numbers' - (pos `mod` length numbers')) numbers'
  where
    (pos, _, numbers') = iterate (round numbers) (0, 0, [0..255]) !! rounds

partOne :: String -> Int
partOne input = let (x0:x1:_) = hash 1 (map read (splitOn "," input)) in x0 * x1

partTwo :: String -> String
partTwo = hexStr . dense . hash 64 . (++ [17,31,73,47,23]) . map ord . takeWhile (/= '\n')
  where
    dense :: [Int] -> [Int]
    dense = map (foldr1 xor) . chunksOf 16

    hexStr :: [Int] -> String
    hexStr = concatMap (\x -> map intToDigit [x `shiftR` 4, x .&. 0x0f])

main :: IO ()
main = do
    input <- getContents
    print (partOne input)
    putStrLn (partTwo input)
