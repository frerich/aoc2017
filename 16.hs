import Text.Parsec

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char deriving Show
type Dance = [Move]

parseDance :: String -> Dance
parseDance = either (error . show) id . parse dance ""
  where
    dance    = move `sepBy` char ','
    move     = spin <|> exchange <|> partner
    spin     = Spin <$> (char 's' *> decimal)
    exchange = Exchange <$> (char 'x' *> decimal) <*> (char '/' *> decimal)
    partner  = Partner <$> (char 'p' *> lower) <*> (char '/' *> lower)
    decimal  = read <$> many1 digit

move :: String -> Move -> String
move p (Spin x)       = let (a, b) = splitAt (length p - x) p in b ++ a
move p (Exchange a b) = move p (Partner (p !! a) (p !! b))
move p (Partner a b)  = map (swap a b) p
  where
    swap a b x
      | x == a    = b
      | x == b    = a
      | otherwise = x

partOne :: Dance -> String -> String
partOne dance p = foldl move p dance

partTwo :: Dance -> String -> String
partTwo dance p = iterate (partOne dance) p !! iterations
  where
    period = let (x:xs) = iterate (partOne dance) p in 1 + length (takeWhile (/= x) xs)
    iterations = 1000000000 `rem` period

main :: IO ()
main = do
    dance <- parseDance <$> getContents
    putStrLn (partOne dance ['a'..'p'])
    putStrLn (partTwo dance ['a'..'p'])

