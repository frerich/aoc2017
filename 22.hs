import Data.Map (Map)
import qualified Data.Map as Map

data Dir = North | South | West | East
data State = Clean | Infected deriving Eq
type Pos = (Int, Int)
type Grid = Map Pos State

main :: IO ()
main = do
    (grid, width, height) <- parseGrid <$> getContents
    print (partOne grid width height)

partOne :: Grid -> Int -> Int -> Int
partOne grid width height = count (== Infected) (bursts grid width height)

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

state :: Grid -> Pos -> State
state grid pos = Map.findWithDefault Clean pos grid

infect :: Pos -> Grid -> Grid
infect pos grid = Map.insert pos Infected grid

cleanse :: Pos -> Grid -> Grid
cleanse pos grid = Map.insert pos Clean grid

bursts :: Grid -> Int -> Int -> [State]
bursts grid width height = go 0 (grid, center, North) []
  where
    center = (width `div` 2, height `div` 2)

    go n (grid, pos, dir) acc
        | n == 10000 = acc
        | otherwise = go (n + 1) (update pos grid, forward dir' pos, dir') (ev : acc)
      where
        (update, dir', ev)
            | state grid pos == Infected = (cleanse, turnEast dir, Clean)
            | otherwise                 = (infect, turnWest dir, Infected)


parseGrid :: String -> (Grid, Int, Int)
parseGrid s = (Map.fromList [(p,Infected) | (p,'#') <- grid], width, height)
  where
    rows = lines s
    width = length (head rows)
    height = length rows
    grid = concat (zipWith (\y -> zipWith (\x c -> ((x,y),c)) [0..]) [0..] rows)

count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

