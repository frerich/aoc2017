import Data.List (sortBy, sort, group, find)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Text.Parsec

parseInput :: String -> Map String (Int, [String])
parseInput input = case parse (line `sepEndBy` newline) "" input of
    Left  _ -> Map.empty
    Right x -> Map.fromList x
  where
    line = do
      name <- many1 lower
      spaces
      weight <- between (char '(') (char ')') (many1 digit)
      abovePrograms <- option [] (string " -> " >> many1 lower `sepBy` string ", ")
      return (name, (read weight, abovePrograms))

partOne :: Map String (Int, [String]) -> String
partOne m = Set.elemAt 0 (names `Set.difference` referencedNames)
  where
    names = Set.fromList . Map.keys $ m
    referencedNames = Set.unions . map (Set.fromList . snd) . Map.elems $ m

rle :: Ord a => [a] -> [(a, Int)]
rle = map (\xs -> (head xs, length xs)) . group . sort

partTwo :: Map String (Int, [String]) -> Int
partTwo m = let (unbalancedName:parentOfUnbalanced:_) = go (partOne m) []
                [(uncommonValue, _), (commonValue, _)] = sortBy (comparing snd) . rle . map totalWeight . above $ parentOfUnbalanced
             in weight unbalancedName + (commonValue - uncommonValue)
  where
    go s acc = let aboves = above s
                   totalWeights = map totalWeight aboves
                in case find ((== 1) . snd) . rle $ totalWeights of
                        Just (w, 1) -> go name (name : acc) where Just (name, _) = find ((== w) . snd) (zip aboves totalWeights)
                        _           -> acc

    above s = let (_, rs) = fromJust (Map.lookup s m) in rs
    weight s = let (w, _) = fromJust (Map.lookup s m) in w
    totalWeight s = let (w, rs) = fromJust (Map.lookup s m) in w + sum (map totalWeight rs)

main :: IO ()
main = do
    input <- parseInput <$> getContents
    print (partOne input)
    print (partTwo input)
