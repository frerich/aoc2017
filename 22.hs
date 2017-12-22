import Data.Set (Set)
import qualified Data.Set as Set

data Dir = North | South | West | East
type Pos = (Int, Int)
type Grid = Set Pos

main :: IO ()
main = do
    (grid, width, height) <- parseGrid <$> getContents
    print (partOne grid width height)

partOne :: Grid -> Int -> Int -> Int
partOne grid width height = count (== '#') (bursts grid width height)

turnWest :: Dir -> Dir
turnWest North    = West
turnWest West  = South
turnWest South  = East
turnWest East = North

turnEast :: Dir -> Dir
turnEast North    = East
turnEast East = South
turnEast South  = West
turnEast West  = North

forward :: Dir -> Pos -> Pos
forward North    (x,y) = (x,y-1)
forward South  (x,y) = (x,y+1)
forward West  (x,y) = (x-1,y)
forward East (x,y) = (x+1,y)

infected :: Grid -> Pos -> Bool
infected grid pos = pos `Set.member` grid

infect :: Pos -> Grid -> Grid
infect pos grid = pos `Set.insert` grid

cleanse :: Pos -> Grid -> Grid
cleanse pos grid = pos `Set.delete` grid

bursts :: Grid -> Int -> Int -> [Char]
bursts grid width height = go 0 (grid, center, North) []
  where
    center = (width `div` 2, height `div` 2)

    go n (grid, pos, dir) acc
        | n == 10000 = acc
        | otherwise = go (n + 1) (update pos grid, forward dir' pos, dir') (ev : acc)
      where
        (update, dir', ev)
            | infected grid pos = (cleanse, turnEast dir, '.')
            | otherwise         = (infect, turnWest dir, '#')


parseGrid :: String -> (Grid, Int, Int)
parseGrid s = (Set.fromList [p | (p,'#') <- grid], width, height)
  where
    rows = lines s
    width = length (head rows)
    height = length rows
    grid = concat (zipWith (\y -> zipWith (\x c -> ((x,y),c)) [0..]) [0..] rows)

count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

