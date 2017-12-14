import Data.List.Split (splitOn)
import Prelude hiding (round)
import KnotHash

partOne :: String -> Int
partOne input = let (x0:x1:_) = knotHash 1 (map read (splitOn "," input)) in x0 * x1

partTwo :: String -> String
partTwo = knotHashHex

main :: IO ()
main = do
    input <- getContents
    print (partOne input)
    putStrLn (partTwo input)
