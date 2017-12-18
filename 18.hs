import Debug.Trace

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec

data Operand
    = Register Char
    | Number Int
    deriving Show

data Instruction
    = Snd Operand
    | Set Char Operand
    | Add Char Operand
    | Mul Char Operand
    | Mod Char Operand
    | Rcv Char
    | Jgz Operand Operand
    deriving Show

data SoundActivity
    = Play Int
    | Recover Int
    deriving Show

type Program = Vector Instruction
type Memory = Map Char Int
data Machine = Machine Int Memory (Maybe SoundActivity)

parseProgram :: String -> Program
parseProgram = either (error . show) id . parse program ""
  where
    program = Vector.fromList <$> instruction `endBy` newline
    instruction = try snd <|> set <|> add <|> try mul <|> mod <|> rcv <|> jgz
    snd = Snd <$> (string "snd " *> operand)
    set = Set <$> (string "set " *> lower <* space) <*> operand
    add = Add <$> (string "add " *> lower <* space) <*> operand
    mul = Mul <$> (string "mul " *> lower <* space) <*> operand
    mod = Mod <$> (string "mod " *> lower <* space) <*> operand
    rcv = Rcv <$> (string "rcv " *> lower)
    jgz = Jgz <$> (string "jgz " *> operand <* space) <*> operand
    operand = register <|> number
    register = Register <$> lower
    number = Number <$> int
    int = do
        sign <- option id (char '-' >> return negate)
        decimal <- read <$> many1 digit
        return (sign decimal)

eval :: Instruction -> Machine -> Machine
eval instr (Machine ip mem activity) =
    case instr of
        Snd x   -> Machine (ip + 1) mem (Just (Play (value x)))
        Set x y -> Machine (ip + 1) (Map.insert x (value y)                          mem) activity
        Add x y -> Machine (ip + 1) (Map.insert x (value (Register x) + value y)     mem) activity
        Mul x y -> Machine (ip + 1) (Map.insert x (value (Register x) * value y)     mem) activity
        Mod x y -> Machine (ip + 1) (Map.insert x (value (Register x) `rem` value y) mem) activity
        Rcv x   -> if value (Register x) /= 0
                    then let Just (Play freq) = activity in Machine (ip + 1) mem (Just (Recover freq))
                    else Machine (ip + 1) mem activity
        Jgz x y -> if value x > 0
                    then Machine (ip + value y) mem activity
                    else Machine (ip + 1) mem activity
  where
    value (Register r) = Map.findWithDefault 0 r mem
    value (Number x)   = x

partOne :: Program -> Int
partOne program = go (Machine 0 Map.empty Nothing)
  where
    go (Machine _ _ (Just (Recover freq))) = freq
    go machine@(Machine ip mem activity)   =
--        trace ("Memory: " ++ show mem ++ "; evaluating <" ++ show (program Vector.! ip) ++ ">") $
        go (eval (program Vector.! ip) machine)

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print (partOne program)
