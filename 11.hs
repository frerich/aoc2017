import Data.List.Split (splitOn)

move :: (Int, Int) -> String -> (Int, Int)
move (x, y) s = case s of
    "nw" -> (x - 2, y - 1)
    "n"  -> (x,     y - 2)
    "ne" -> (x + 2, y - 1)
    "se" -> (x + 2, y + 1)
    "s"  -> (x,     y + 2)
    "sw" -> (x - 2, y + 1)
    _    -> (x,     y)

distance :: (Int, Int) -> Int
distance (x, y) = if abs x > abs y * 2 then abs x `div` 2 else abs y `div` 2

partOne :: [String] -> Int
partOne = distance . foldl move (0,0)

partTwo :: [String] -> Int
partTwo = maximum . map distance . scanl move (0,0)

main :: IO ()
main = do
    input <- splitOn "," . takeWhile (/= '\n') <$> getContents
    print (partOne input)
    print (partTwo input)
