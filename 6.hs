import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (sortBy, groupBy)

rotateBy :: Int -> [a] -> [a]
rotateBy 1 (x:xs) = xs ++ [x]
rotateBy n xs = take (length xs) (drop n (cycle xs))

distribute :: Int -> [(Int, Int)] -> [(Int, Int)]
distribute _ [] = []
distribute 0 xs = xs
distribute n ((i,v):xs) = distribute (n - 1) (rotateBy 1 ((i,v+1):xs))

selectBank :: [(Int, Int)] -> (Int, Int)
selectBank = head . last . groupBy ((==) `on` snd) . sortBy (comparing snd)

step :: [Int] -> [Int]
step xs = let ((ri,rv):rr) = rotateBy maxBankIdx es in map snd . sortBy (comparing fst) $ distribute rv (rotateBy 1 ((ri,0):rr))
  where
    maxBankIdx = fst (selectBank es)
    es = zip [0..] xs

takeWhileUnseen :: Ord a => [a] -> (Maybe a, Set a)
takeWhileUnseen = go Set.empty
  where
    go seen (x:xs) = if x `Set.member` seen then (Just x, seen) else go (Set.insert x seen) xs
    go seen _      = (Nothing, seen)

parse :: String -> [Int]
parse = map read . words

partOne :: [Int] -> Int
partOne = Set.size . snd . takeWhileUnseen . iterate step

partTwo :: [Int] -> Int
partTwo xs = let [(i,_),(j,_)] = take 2 . filter (\(_,v) -> v == dupe) . zip [0..] $ steps in j - i
  where
    (Just dupe, _) = takeWhileUnseen steps
    steps = iterate step xs

main :: IO ()
main = interact $ show . partTwo . parse
