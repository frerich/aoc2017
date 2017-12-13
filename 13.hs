import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (transpose, tails)

periodic :: Int -> [Int]
periodic 0 = repeat 0
periodic n = cycle (1 : replicate (2 * (n - 1) - 1) 0)

topRowStates :: [Int] -> [[Int]]
topRowStates = transpose . map periodic

severities :: [Int] -> [Int]
severities = zipWith (*) [0..]

collisions :: Int -> [[Int]] -> [Int]
collisions n states = map (\i -> states !! i !! i) [0..n - 1]

parse :: String -> Map Int Int
parse = Map.fromList . map parseLine . lines
  where
    parseLine s = let [d, r] = words s in (read (init d), read r)

expand :: Map Int Int -> [Int]
expand m
    | Map.null m = []
    | otherwise  = map (\i -> Map.findWithDefault 0 i m) [0..maximum (Map.keys m)]

partOne :: [Int] -> Int
partOne ranges = sum (zipWith (*) (collisions (length ranges) (topRowStates ranges)) (severities ranges))

partTwo :: [Int] -> Int
partTwo ranges = length . takeWhile (any (/= 0) . collisions (length ranges)) . tails . topRowStates $ ranges

main :: IO ()
main = do
    input <- expand . parse <$> getContents
    print (partOne input)
    print (partTwo input)
