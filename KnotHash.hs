module KnotHash (knotHash, knotHashHex) where

import Data.Bits (xor, shiftR, (.&.))
import Data.Char (ord, intToDigit)
import Data.List.Split (chunksOf)
import Prelude hiding (round)

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

round :: [Int] -> (Int, Int, [Int]) -> (Int, Int, [Int])
round = flip (foldl go)
  where
    go (pos, skipSize, xs) len = (pos + len + skipSize, skipSize + 1, rotate (len + skipSize) (step len xs))

    step :: Int -> [a] -> [a]
    step len xs = let (as, bs) = splitAt len xs in reverse as ++ bs

knotHash :: Int -> [Int] -> [Int]
knotHash rounds numbers = rotate (length numbers' - (pos `mod` length numbers')) numbers'
  where
    (pos, _, numbers') = iterate (round numbers) (0, 0, [0..255]) !! rounds

knotHashHex :: String -> String
knotHashHex = hexStr . dense . knotHash 64 . (++ [17,31,73,47,23]) . map ord . takeWhile (/= '\n')
  where
    dense :: [Int] -> [Int]
    dense = map (foldr1 xor) . chunksOf 16

    hexStr :: [Int] -> String
    hexStr = concatMap (\x -> map intToDigit [x `shiftR` 4, x .&. 0x0f])

