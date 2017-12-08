import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Text.Parsec

type Relation = Int -> Int -> Bool
type Register = String
data Condition = Condition Register Relation Int
type Operation = Int -> Int -> Int
data Instruction = Instruction Register Operation Int Condition
type Program = [Instruction]
type Memory = Map Register Int

parseInput :: String -> Program
parseInput input = case parse program "" input of
    Left err -> trace ("PARSE ERROR: " ++ show err) []
    Right p -> p
  where
    program = instruction `sepEndBy` newline
    instruction = Instruction <$> register <*> operation <*> int <*> (string " if " *> condition)
    int = do
        sign <- option id (char '-' >> return negate)
        digits <- read <$> many1 digit
        return (sign digits)
    register = many1 lower
    operation = (try (string " inc ") >> return (+))
             <|> (try (string " dec ") >> return (-))
    condition = Condition <$> register <*> relation <*> int
    relation = (try (string " < ") >> return (<))
             <|> (try (string " > ") >> return (>))
             <|> (try (string " == ") >> return (==))
             <|> (try (string " != ") >> return (/=))
             <|> (try (string " <= ") >> return (<=))
             <|> (try (string " >= ") >> return (>=))

holds :: Condition -> Memory -> Bool
holds (Condition reg rel val) mem = rel (Map.findWithDefault 0 reg mem) val

exec :: Instruction -> Memory -> Memory
exec (Instruction reg op val cond) mem
    | cond `holds` mem = let val' = op (Map.findWithDefault 0 reg mem) val in Map.insert reg val' mem
    | otherwise        = mem

maxRegisterVal :: Memory -> Int
maxRegisterVal = maximum . Map.elems

partOne :: Program -> Int
partOne p = maxRegisterVal (run Map.empty p)
  where
    run = foldl' (flip exec)

partTwo :: Program -> Int
partTwo p = snd (run (Map.empty, 0) p)
  where
    run = foldl' (\(mem, maxVal) i -> let mem' = exec i mem in (mem', max maxVal (maxRegisterVal mem')))

main :: IO ()
main = do
    program <- parseInput <$> getContents
    print (partOne program)
    print (partTwo program)

