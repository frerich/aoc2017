{-# LANGUAGE BangPatterns #-}
import           Data.Map (Map)
import qualified Data.Map as Map


data Dir = North | South | West | East
data State = Clean | Weakened | Infected | Flagged deriving Eq
type Pos = (Int, Int)
type Grid = Map Pos State


main :: IO ()
main = do
    (grid, width, height) <- parseGrid <$> getContents
    let center = (width `div` 2, height `div` 2)
    print (partOne grid center)
    print (partTwo grid center)


partOne :: Grid -> Pos -> Int
partOne grid pos = count (== Infected) (simulate 10000 step grid pos North)
  where
    step grid pos =
        case state grid pos of
            Infected  -> (Clean   , turnRight)
            _         -> (Infected, turnLeft)


partTwo :: Grid -> Pos -> Int
partTwo grid pos = count (== Infected) (simulate 10000000 step grid pos North)
  where
    step grid pos =
        case state grid pos of
            Clean    -> (Weakened, turnLeft)
            Weakened -> (Infected, id)
            Infected -> (Flagged , turnRight )
            Flagged  -> (Clean   , turnLeft . turnLeft )


-- | Adjust a direction to the left
turnLeft :: Dir -> Dir
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North


-- | Adjust a direction to the right (which is the same as turning left three times)
turnRight :: Dir -> Dir
turnRight = turnLeft . turnLeft . turnLeft


-- | Update a position according to the current direction
forward :: Dir -> Pos -> Pos
forward North (x, y) = (x,     y - 1)
forward South (x, y) = (x,     y + 1)
forward West  (x, y) = (x - 1, y)
forward East  (x, y) = (x + 1, y)


-- | Yield the state of a cell in the infinite grid
state :: Grid -> Pos -> State
state grid pos = Map.findWithDefault Clean pos grid


-- | Run the grid simulation a number of times using an initial grid and a 'burst' function
simulate :: Int -> (Grid -> Pos -> (State, Dir -> Dir)) -> Grid -> Pos -> Dir -> [State]
simulate numBursts step = go 0 []
  where
    go !n !acc !grid !pos !dir
        | n == numBursts = acc
        | otherwise     = let dir' = turn dir
                           in go (n + 1) (flag : acc) (Map.insert pos flag grid) (forward dir' pos) dir'
      where
        (flag, turn) = step grid pos


-- | Given a string, return an grid of cells as well as the dimensions of the grid
parseGrid :: String -> (Grid, Int, Int)
parseGrid s = (Map.fromList [(p, Infected) | (p, '#') <- grid], width, height)
  where
    rows = lines s
    width = length (head rows)
    height = length rows
    grid = concat (zipWith (\y -> zipWith (\x c -> ((x,y),c)) [0..]) [0..] rows)


-- | Yield the number of elements in a given sequence matching the given condition
count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

