import Text.Parsec
import Data.Ord (comparing)
import Data.List (minimumBy, sortBy, groupBy)
import Data.Function (on)

type Coord = (Int, Int, Int)

data Particle = Particle
    { particlePos :: Coord
    , particleVel :: Coord
    , particleAcc :: Coord
    }

parseInput :: String -> [Particle]
parseInput = either (error . show) id . parse (particle `endBy` newline) ""
  where
    particle = Particle <$> position <*> (string ", " *> velocity) <*> (string ", " *> acceleration)
    position = string "p=" >> coord
    velocity = string "v=" >> coord
    acceleration = string "a=" >> coord
    coord = do
        char '<'
        [x,y,z] <- int `sepBy` char ','
        char '>'
        return (x,y,z)
    int = do
        sign <- option id (char '-' >> return negate)
        digits <- read <$> many1 digit
        return (sign digits)

partOne :: [Particle] -> Int
partOne = fst . minimumBy (ordering `on` snd) . zip [0..]
  where
    ordering = mconcat [ comparing (manhattan . particleAcc)
                       , comparing (manhattan . particleVel)
                       , comparing (manhattan . particlePos)]
    manhattan (x, y, z) = abs x + abs y + abs z

partTwo :: [Particle] -> Int
partTwo = undefined

main :: IO ()
main = do
    input <- parseInput <$> getContents
    print (partOne input)

