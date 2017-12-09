import Text.Parsec
import Text.Parsec.String

data Group = Group [Group] | Garbage String deriving Show

unescapedChar :: Parser Char
unescapedChar = do
    ch <- anyChar
    if ch == '!'
    then anyChar >> return '!'
    else return ch

garbage :: Parser Group
garbage = Garbage <$> (char '<' *> unescapedChar `manyTill` char '>')

group :: Parser Group
group = Group <$> between (char '{') (char '}') ((garbage <|> group) `sepBy` char ',')

parseInput :: String -> Group
parseInput s = case parse group "" s of
    Left _ -> Group []
    Right g -> g

score :: Group -> Int
score = go 1
  where
    go i (Group xs)  = i + sum (map (go (i + 1)) xs)
    go _ (Garbage _) = 0

garbageLen :: Group -> Int
garbageLen (Group xs)  = sum (map garbageLen xs)
garbageLen (Garbage s) = length (filter (/= '!') s)

main :: IO ()
main = do
    g <- parseInput <$> getContents
    print (score g)
    print (garbageLen g)
