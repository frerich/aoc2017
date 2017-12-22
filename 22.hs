{-# LANGUAGE BangPatterns #-}
import Data.Map (Map)
import qualified Data.Map as Map

data Dir = North | South | West | East
data State = Clean | Weakened | Infected | Flagged deriving Eq
type Pos = (Int, Int)
type Grid = Map Pos State

main :: IO ()
main = do
    (grid, width, height) <- parseGrid <$> getContents
    print (partOne grid width height)
    print (partTwo grid width height)

partOne :: Grid -> Int -> Int -> Int
partOne grid width height = count (== Infected) (bursts grid width height burst 10000)
  where
    burst grid pos dir
        | state grid pos == Infected = (Clean, turnRight dir)
        | otherwise                 = (Infected, turnLeft dir)

partTwo :: Grid -> Int -> Int -> Int
partTwo grid width height = count (== Infected) (bursts grid width height burst 10000000)
  where
    burst grid pos dir =
        case state grid pos of
            Clean -> (Weakened, turnLeft dir)
            Weakened -> (Infected, dir)
            Infected -> (Flagged, turnRight dir)
            Flagged -> (Clean, turnLeft (turnLeft dir))

turnLeft :: Dir -> Dir
turnLeft North    = West
turnLeft West  = South
turnLeft South  = East
turnLeft East = North

turnRight :: Dir -> Dir
turnRight North    = East
turnRight East = South
turnRight South  = West
turnRight West  = North

forward :: Dir -> Pos -> Pos
forward North    (x,y) = (x,y-1)
forward South  (x,y) = (x,y+1)
forward West  (x,y) = (x-1,y)
forward East (x,y) = (x+1,y)

state :: Grid -> Pos -> State
state grid pos = Map.findWithDefault Clean pos grid

bursts :: Grid -> Int -> Int -> (Grid -> Pos -> Dir -> (State, Dir)) -> Int -> [State]
bursts grid width height burst numBursts = go 0 (grid, center, North) []
  where
    center = (width `div` 2, height `div` 2)

    go !n (!grid, !pos, !dir) !acc
        | n == numBursts = acc
        | otherwise = go (n + 1) (Map.insert pos flag grid, forward dir' pos, dir') (flag : acc)
      where
        (flag, dir') = burst grid pos dir


parseGrid :: String -> (Grid, Int, Int)
parseGrid s = (Map.fromList [(p,Infected) | (p,'#') <- grid], width, height)
  where
    rows = lines s
    width = length (head rows)
    height = length rows
    grid = concat (zipWith (\y -> zipWith (\x c -> ((x,y),c)) [0..]) [0..] rows)

count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

