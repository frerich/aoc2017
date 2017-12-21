import Data.Map (Map)
import qualified Data.Map as M
import Data.Array (Array)
import qualified Data.Array as A
import Data.List (groupBy, transpose)
import Data.List.Split (splitOn)

type Grid = Array (Int, Int) Char
type Rules = Map Grid Grid

parseRules :: String -> Rules
parseRules = M.fromList . map (\(a,b) -> (parseGrid a, parseGrid b)) . concatMap (variants . parseLine) . lines
  where
    parseLine :: String -> ([String], [String])
    parseLine s = let [from, to] = splitOn " => " s in (splitOn "/" from, splitOn "/" to)

    variants :: ([String], [String]) -> [([String], [String])]
    variants (a, b) = [(a', b) | a' <- take 4 (iterate rotate a) ++ take 4 (iterate rotate (reverse a))]

    rotate :: [[a]] -> [[a]]
    rotate = transpose . reverse

    parseGrid :: [String] -> Grid
    parseGrid rows = A.listArray ((0,0), (length rows - 1, length rows - 1)) (concat rows)

chunk :: Int -> Int -> Int -> Grid -> Grid
chunk size rowOffs colOffs grid = A.listArray ((0,0), (size - 1, size - 1)) . map (grid A.!) $ indices
  where
    indices = [(rowOffs + row, colOffs + col) | row <- [0..size - 1], col <- [0..size - 1]]

gridSize :: Grid -> Int
gridSize grid = let ((0, 0), (maxRow, _)) = A.bounds grid in maxRow + 1

breakGrid :: Grid -> [[Grid]]
breakGrid grid
  | even size = map (rowChunks 2) [0,2..size-1]
  | otherwise = map (rowChunks 3) [0,3..size-1]
  where
    size = gridSize grid

    rowChunks :: Int -> Int -> [Grid]
    rowChunks step row = [chunk step row col grid | col <- [0,step..size-1]]

enhance :: Rules -> [[Grid]] -> [[Grid]]
enhance rules = map (map (\g -> rules M.! g))

joinGrid :: [[Grid]] -> Grid
joinGrid tiles = A.array ((0,0), (length tiles * tileSize - 1, length tiles * tileSize - 1)) (concatMap shiftedAssocs anchoredTiles)
  where
    tileSize :: Int
    tileSize = gridSize (tiles !! 0 !! 0)

    anchorRow :: Int -> [Grid] -> [((Int, Int), Grid)]
    anchorRow rowNum = zipWith (\col grid -> ((rowNum, col), grid)) [0,tileSize..]

    anchoredTiles :: [((Int, Int), Grid)]
    anchoredTiles = concat (zipWith anchorRow [0,tileSize..] tiles)

    shiftedAssocs :: ((Int, Int), Grid) -> [((Int, Int), Char)]
    shiftedAssocs ((rowOffs, colOffs), grid) = map (\((row, col), c) -> ((row + rowOffs, col + colOffs), c)) (A.assocs grid)


step :: Rules -> Grid -> Grid
step rules = joinGrid . enhance rules . breakGrid

image :: Grid
image = A.listArray ((0,0), (2,2)) $ concat
    [ ".#."
    , "..#"
    , "###"
    ]

pixelsAfter :: Int -> Rules -> Int
pixelsAfter n rules = length . filter (== '#') . A.elems $ iterate (step rules) image !! n

partOne :: Rules -> Int
partOne = pixelsAfter 5

partTwo :: Rules -> Int
partTwo = pixelsAfter 18

main :: IO ()
main = do
    rules <- parseRules <$> getContents
    print (partOne rules)
    print (partTwo rules)
