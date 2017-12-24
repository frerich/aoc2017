import Data.List (inits, tails, maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

type Port = Int
type Component = (Port, Port)
type Bridge = [Component]

main :: IO ()
main = do
    input <- parseInput <$> getContents
    print (partOne input)
    print (partTwo input)

parseInput :: String -> [Component]
parseInput = map parseLine . lines
  where
    parseLine s = let [a, b] = splitOn "/" s in (read a, read b)

partOne :: [Component] -> Int
partOne components = strength (maximumBy (comparing strength) (bridges components 0))

partTwo :: [Component] -> Int
partTwo components = strength (maximumBy (mconcat [comparing length, comparing strength]) (bridges components 0))

bridges :: [Component] -> Port -> [Bridge]
bridges tiles port = concatMap (\(c, rest) -> [c] : map (c :) (bridges rest (otherPort c))) next
  where
    next = [(c, rest) | (c, rest) <- pick tiles, port `fits` c]
    otherPort (x, y) = if x == port then y else x
    fits n (x, y) = n == x || n == y

strength :: Bridge -> Int
strength bridge = sum (map (\(x, y) -> x + y) bridge)

pick :: [a] -> [(a,[a])]
pick as = [(y, xs ++ ys) | (xs, (y:ys)) <- zip (inits as) (tails as)]

