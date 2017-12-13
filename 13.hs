import Data.Array (listArray, elems, (//))

topRowState :: [Int] -> Int -> [Int]
topRowState ranges picoSec = map stateAt ranges
  where
    stateAt 0 = 0
    stateAt range = if picoSec `mod` (2 * (range - 1)) == 0 then 1 else 0

collisionsAt :: Int -> [Int] -> Int -> [Int]
collisionsAt len ranges picoSec = zipWith (!!) states [0..len - 1]
   where
     states = map (topRowState ranges) [picoSec..picoSec + len]

parse :: String -> [Int]
parse s = elems (listArray (0, maximum (map fst parsedMap)) (repeat 0) // parsedMap)
  where
    parsedMap = map parseLine (lines s)
    parseLine l = let [d, r] = words l in (read (init d), read r)

partOne :: [Int] -> Int
partOne ranges = sum (zipWith (*) collisions severities)
  where
    collisions = collisionsAt (length ranges) ranges 0
    severities = zipWith (*) [0..] ranges

partTwo :: [Int] -> Int
partTwo ranges = head . filter (all (== 0) . collisionsAt (length ranges) ranges) $ [0..]

main :: IO ()
main = do
    input <- parse <$> getContents
    print (partOne input)
    print (partTwo input)
