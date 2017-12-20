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

step :: Particle -> Particle
step (Particle (px,py,pz) (vx,vy,vz) (ax,ay,az)) = Particle p' v' (ax,ay,az)
  where
    v'@(v'x, v'y, v'z) = (vx + ax, vy + ay, vz + az)
    p' = (px + v'x, py + v'y, pz + v'z)

partOne :: [Particle] -> Int
partOne = fst . minimumBy (comparing (manhattan . particlePos . snd)) . (!! 1000) . iterate (map (\(i, p) -> (i, step p))) . zip [0..]
  where
    manhattan (x, y, z) = abs x + abs y + abs z

partTwo :: [Particle] -> Int
partTwo = length . (!! 1000) . iterate go
  where
    go ps = map step ps'
      where
        ps' = concat . filter (null . tail) . groupBy ((==) `on` particlePos) . sortBy (comparing particlePos) $ ps

main :: IO ()
main = do
    input <- parseInput <$> getContents
    print (partOne input)
    print (partTwo input)

