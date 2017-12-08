import Data.Char (digitToInt, isDigit)

main :: IO ()
main = do
    input <- takeWhile (isDigit) <$> getContents
    print (solve 1 input)
    print (solve (length input `div` 2) input)

solve :: Int -> String -> Int
solve n xs = sum [digitToInt a | (a,b) <- zip xs (drop n $ cycle xs), a == b]

