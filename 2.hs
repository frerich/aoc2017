type Spreadsheet = [[Int]]

parse :: String -> Spreadsheet
parse = map (map read . words) . lines

partOne :: Spreadsheet -> Int
partOne = sum . map (\row -> maximum row - minimum row)

evenlyDivisible :: [Int] -> [(Int,Int)]
evenlyDivisible xs = [(x,y) | x <- xs, y <- xs, x > y && x `mod` y  == 0]

partTwo :: Spreadsheet -> Int
partTwo = sum . map (\row -> let (x,y) = head (evenlyDivisible row) in x `div` y)

main :: IO ()
main = do
    input <- parse <$> getContents
    print (partOne input)
    print (partTwo input)

