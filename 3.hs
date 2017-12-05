import qualified Data.Map.Lazy as M
import Data.Maybe (mapMaybe)

data Direction = North | West | South | East deriving (Show, Eq, Ord, Bounded, Enum)

spiral :: [Direction]
spiral = concatMap (\n -> drop 1 (concatMap (replicate n) [North .. East]) ++ [East]) [0,2..]

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) dir = case dir of
    North -> (x, y - 1)
    West  -> (x - 1, y)
    South -> (x, y + 1)
    East  -> (x + 1, y)

indices :: [(Int, Int)]
indices = scanl move (0,0) spiral

partOne :: Int -> Int
partOne n = let (x,y) = indices !! (n - 1) in abs x + abs y

valuesPartTwo :: [Int]
valuesPartTwo = go (M.singleton (0,0) 1) indices
  where
    go m (i:is) = let val = valueAt i m in val : go (M.insert i val m) is
    go _ []     = []
    valueAt i m = sum $ mapMaybe (`M.lookup` m) (neighbors i)
    neighbors (x,y) = [(x-1,y-1),(x,y-1),(x+1,y-1),
                       (x-1,y  ),(x,y),  (x+1,y  ),
                       (x-1,y+1),(x,y+1),(x+1,y+1)]

partTwo :: Int -> Int
partTwo n = head (dropWhile (<= n) valuesPartTwo)

main :: IO ()
main = do
    let input = 312051
    print (partOne input)
    print (partTwo input)
