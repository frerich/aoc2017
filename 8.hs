import Debug.Trace (trace)
import Text.Parsec
import Data.Map (Map)
import qualified Data.Map as Map

data Relation = RelLT | RelGT | RelEQ | RelNE | RelLE | RelGE deriving Show
type Register = String
data Condition = Condition Register Relation Int deriving Show
data Operation = Inc | Dec deriving Show
data Instruction = Instruction Register Operation Int Condition deriving Show
type Program = [Instruction]

type Memory = Map Register Int

parseInput :: String -> Program
parseInput input = case parse program "" input of
    Left err -> trace ("PARSE ERROR: " ++ show err) []
    Right p -> p
  where
    program = instruction `sepEndBy` newline
    instruction = do
        reg <- register
        op <- operation
        arg <- int
        string " if "
        cond <- condition
        return (Instruction reg op arg cond)
    int = do
        sign <- option id (char '-' >> return negate)
        digits <- read <$> many1 digit
        return (sign digits)
    register = many1 lower
    operation = (try (string " inc ") >> return Inc)
             <|> (try (string " dec ") >> return Dec)
    condition = Condition <$> register <*> relation <*> int
    relation = (try (string " < ") >> return RelLT)
             <|> (try (string " > ") >> return RelGT)
             <|> (try (string " == ") >> return RelEQ)
             <|> (try (string " != ") >> return RelNE)
             <|> (try (string " <= ") >> return RelLE)
             <|> (try (string " >= ") >> return RelGE)

relFn :: Ord a => Relation -> (a -> a -> Bool)
relFn rel = case rel of
    RelLT -> (<)
    RelGT -> (>)
    RelEQ -> (==)
    RelNE -> (/=)
    RelLE -> (<=)
    RelGE -> (>=)

opFn :: Num a => Operation -> (a -> a -> a)
opFn op = case op of
    Inc -> (+)
    Dec -> (-)

condHolds :: Condition -> Memory -> Bool
condHolds (Condition reg rel val) mem = (relFn rel) (get reg mem) val

get :: Register -> Memory -> Int
get reg mem = Map.findWithDefault 0 reg mem

put :: Register -> Int -> Memory -> Memory
put reg val mem = Map.insert reg val mem

exec :: Instruction -> Memory -> Memory
exec (Instruction reg op val cond) mem
    | condHolds cond mem = put reg ((opFn op) (get reg mem) val) mem
    | otherwise          = mem

partOne :: Program -> Int
partOne p = maximum (Map.elems (run p Map.empty))
  where
    run :: Program -> Memory -> Memory
    run [] mem = mem
    run (i:is) mem = run is (exec i mem)

partTwo :: Program -> Int
partTwo p = snd (run p (Map.empty, 0))
  where
    run :: Program -> (Memory, Int) -> (Memory, Int)
    run [] acc = acc
    run (i:is) (mem, maxVal) = let mem' = exec i mem in run is (mem', max (maximum (Map.elems mem')) maxVal)

main :: IO ()
main = do
    program <- parseInput <$> getContents
    print (partOne program)
    print (partTwo program)

