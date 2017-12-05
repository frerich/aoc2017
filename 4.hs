import Data.List (group, sort)

unique :: Ord a => [a] -> Bool
unique = all (null . tail) . group . sort

validPartOne :: String -> Bool
validPartOne = unique . words

validPartTwo :: String -> Bool
validPartTwo = unique . map sort . words

solution :: (String -> Bool) -> String -> Int
solution p = length . filter p . lines

main :: IO ()
main = do
    input <- getContents
    print (solution validPartOne input)
    print (solution validPartTwo input)
