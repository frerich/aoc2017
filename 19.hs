import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (catMaybes)

type Diagram = Map (Int, Int) Char

parseInput :: String -> Diagram
parseInput = Map.fromList . catMaybes . concat . zipWith (\y line -> zipWith (parseChar y) [0..] line) [0..] . lines
  where
    parseChar y x ch
        | ch `elem` "+-|" || ch `elem` ['A'..'Z'] = Just ((x,y), ch)
        | otherwise                    = Nothing

walk :: Diagram -> [(Int, Int)]
walk d = start : go start (-1, -1)
  where
    go pos prevPos =
        case filter (\p -> p /= prevPos && p `Map.member` d) (adjacent pos) of
            []     -> []
            (p:ps) -> let pos' = if null ps then p else extrapolate prevPos pos
                     in pos' : go pos' pos

    start = (head (filter (\x -> (x,0) `Map.member` d) [0..]), 0)
    adjacent (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    extrapolate (px,py) (qx,qy) = (qx + (qx - px), qy + (qy - py))

partOne :: Diagram -> String
partOne d = [ch | Just ch  <- map (`Map.lookup` d) (walk d), ch `elem` ['A'..'Z']]

partTwo :: Diagram -> Int
partTwo = length . walk

main :: IO ()
main = do
    diagram <- parseInput <$> getContents
    putStrLn (partOne diagram)
    print (partTwo diagram)

