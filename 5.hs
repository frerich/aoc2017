import Data.List (unfoldr)

data Zipper a = Zipper [a] [a] deriving Show

parse :: String -> Zipper Int
parse = Zipper [] . map read . lines

step :: (Int -> Int) -> Zipper Int -> Maybe (Zipper Int)
step adjust (Zipper ls rs@(r:rx))
    | r < 0 = if abs r > length ls
                then Nothing
                else let rs' = adjust r : rx in Just $ Zipper (drop (abs r) ls) (reverse (take (abs r) ls) ++ rs')
    | r >= 0 = if r + 1 > length rs
                then Nothing
                else let rs' = adjust r : rx in Just $ Zipper (reverse (take r rs') ++ ls) (drop r rs')

walk :: (Int -> Int) -> Zipper Int -> [Zipper Int]
walk adjust z = z : unfoldr (\x -> case step adjust x of Nothing -> Nothing; Just x' -> Just (x', x')) z

partOne :: String -> Int
partOne = length . walk (+1) . parse

partTwo :: String -> Int
partTwo = length . walk (\x -> if x >= 3 then x - 1 else x + 1) . parse

main :: IO ()
main = interact $ show . partOne
