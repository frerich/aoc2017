import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

rotate :: Int -> [a] -> [a]
rotate n xs = bs ++ as where (as, bs) = splitAt n xs

distribute :: [Int] -> [Int]
distribute [] = []
distribute xs@(x:_) = zipWith (+) xs summands
  where
    summands
        | x < length xs = (-x) : replicate x 1 ++ repeat 0
        | otherwise     = let (q, r) = x `divMod` length xs in (q - x) : replicate r (q + 1) ++ repeat q

largestBankIdx :: [Int] -> Int
largestBankIdx = fst . foldr1 (\(i,x) (j,y) -> if x >= y then (i,x) else (j,y)) . zip [0..]

step :: [Int] -> [Int]
step xs = rotate (length xs - maxBankIdx) . distribute . rotate maxBankIdx $ xs
  where
    maxBankIdx = largestBankIdx xs

takeWhileUnseen :: Ord a => [a] -> Set a
takeWhileUnseen = go Set.empty
  where
    go seen (x:xs)
        | x `Set.member` seen = seen
        | otherwise           = go (Set.insert x seen) xs
    go seen _      = seen

partOne :: [Int] -> Int
partOne = Set.size . takeWhileUnseen . iterate step

partTwo :: [Int] -> Int
partTwo = go Map.empty . zip [0..] . iterate step
  where
    go seen ((i,x):xs) =
        case Map.lookup x seen of
            Just idx -> i - idx
            Nothing  -> go (Map.insert x i seen) xs
    go _ []            = -1

main :: IO ()
main = do
    input <- map read . words <$> getContents
    print (partOne input)
    print (partTwo input)
