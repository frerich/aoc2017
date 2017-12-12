import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec

parseInput :: String -> Map Int (Set Int)
parseInput s = either (error . show) Map.fromList $ parse (line `endBy` newline) "" s
  where
    int = read <$> many1 digit
    line = (,) <$> int <* string " <-> " <*> (Set.fromList <$> int `sepBy` string ", ")

members :: Int -> Map Int (Set Int) -> Set Int
members i m = go Set.empty i
  where
    go seen x = let unseen = fromJust (Map.lookup x m) `Set.difference` seen
                    seen' = Set.insert x seen
                in Set.insert x seen `Set.union` Set.foldl Set.union Set.empty (Set.map (go seen') unseen)

groups :: Map Int (Set Int) -> [Set Int]
groups = go []
  where
    go acc m
        | Map.null m = acc
        | otherwise  = let g = members (head (Map.keys m)) m in go (g : acc) (m `withoutKeys` g)
    withoutKeys m s = Map.filterWithKey (\k _ -> k `Set.notMember` s) m

partOne :: Map Int (Set Int) -> Int
partOne = Set.size . members 0

partTwo :: Map Int (Set Int) -> Int
partTwo = length . groups

main :: IO ()
main = do
    input <- parseInput <$> getContents
    print (partOne input)
    print (partTwo input)
