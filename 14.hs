import KnotHash
import Data.Array (Array, listArray, indices, (!), bounds)
import Data.Char (digitToInt)
import Data.Graph
import Text.Printf (printf)

disk :: String -> [String]
disk s = map (concatMap (printf "%04b" . digitToInt) . rowHexRep) [0..127]
  where
    rowHexRep :: Int -> String
    rowHexRep i = knotHashHex (s ++ "-" ++ show i)

buildGraph :: Array (Int, Int) Char -> Graph
buildGraph arr = let (g, _, _) = graphFromEdges [(i, i, adjacent i) | i <- indices arr, isSet i] in g
  where
    isSet i = arr ! i == '1'
    adjacent (row, col) = filter isSet [ (max 0 (row - 1), col)
                                       , (min 127 (row + 1), col)
                                       , (row, max 0 (col - 1))
                                       , (row, min 127 (col + 1))
                                       ]

partOne :: [String] -> Int
partOne = sum . map (length . filter (== '1'))

partTwo :: [String] -> Int
partTwo = length . components . buildGraph . listArray ((0,0), (127,127)) . concat

main :: IO ()
main = do
    let input = disk "uugsqrei"
    print (partOne input)
    print (partTwo input)
