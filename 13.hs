import qualified Data.Map.Strict as Map

frequency :: Int -> Int
frequency 1 = 1
frequency n = 2 * (n - 1)

topRowState :: [Int] -> Int -> [Int]
topRowState ranges picoSec = map stateAt ranges
  where
    stateAt 0 = 0
    stateAt range = if picoSec `mod` frequency range == 0 then 1 else 0

collisionsAt :: [Int] -> Int -> [Int]
collisionsAt ranges picoSec = zipWith (!!) states [0..length ranges - 1]
   where
     states = map (topRowState ranges) [picoSec..picoSec + length ranges]

parse :: String -> [Int]
parse s = map (\i -> Map.findWithDefault 0 i parsedMap) [0..maximum (Map.keys parsedMap)]
  where
    parsedMap = Map.fromList . map parseLine . lines $ s
    parseLine l = let [d, r] = words l in (read (init d), read r)

partOne :: [Int] -> Int
partOne ranges = sum (zipWith (*) collisions severities)
  where
    collisions = collisionsAt ranges 0
    severities = zipWith (*) [0..] ranges

partTwo :: [Int] -> Int
partTwo ranges = head . filter (all (== 0) . collisionsAt ranges) $ [0..]

main :: IO ()
main = do
    input <- parse <$> getContents
    print (partOne input)
    print (partTwo input)
