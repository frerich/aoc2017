import Debug.Trace

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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

data SoundAction
    = Sound Int
    | Recover Int

type Program = Vector Instruction
type Memory = Map Char Int
type Queue = Seq Int
data Process = Process Queue Int Memory

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

brokenEval :: Instruction -> Process -> (Process, Maybe SoundAction)
brokenEval instr process@(Process input ip mem) =
    case instr of
        Rcv x -> if Map.findWithDefault 0 x mem /= 0
                  then (Process input (ip + 1) mem, Just (Recover (Seq.index input (Seq.length input - 1))))
                  else (Process input (ip + 1) mem, Nothing)
        _     -> eval instr process

eval :: Instruction -> Process -> (Process, Maybe SoundAction)
eval instr (Process input ip mem) =
    case instr of
        Snd x   -> (Process input (ip + 1) mem, Just (Sound (value x)))
        Set x y -> (Process input (ip + 1) (Map.insert x (value y)                          mem), Nothing)
        Add x y -> (Process input (ip + 1) (Map.insert x (value (Register x) + value y)     mem), Nothing)
        Mul x y -> (Process input (ip + 1) (Map.insert x (value (Register x) * value y)     mem), Nothing)
        Mod x y -> (Process input (ip + 1) (Map.insert x (value (Register x) `rem` value y) mem), Nothing)
        Rcv x   -> if Seq.null input
                    then (Process input ip mem, Nothing)
                    else (Process (Seq.drop 1 input) (ip + 1) (Map.insert x (Seq.index input 0) mem), Just (Recover 0))
        Jgz x y -> if value x > 0
                    then (Process input (ip + value y) mem, Nothing)
                    else (Process input (ip + 1) mem, Nothing)
  where
    value (Register r) = Map.findWithDefault 0 r mem
    value (Number x)   = x

partOne :: Program -> Int
partOne program = go (Process Seq.empty 0 Map.empty)
  where
    go process@(Process input ip mem) =
        case brokenEval (program Vector.! ip) process of
            (process'               , Nothing)          -> go process'
            (Process input' ip' mem', Just (Sound x))   -> go (Process (input Seq.|> x) ip' mem')
            (_                      , Just (Recover x)) -> x

partTwo :: Program -> Int
partTwo program = go (processA, id) (processB, (+1)) 0
  where
    go (pa@(Process a_input a_ip a_mem), fa) (pb@(Process b_input b_ip b_mem), fb) cnt
        | suspended pa && suspended pb = cnt
        | otherwise =
        case eval (program Vector.! a_ip) pa of
            (pa', Nothing)          -> go (pb, fb) (pa', fa) cnt
            (pa', Just (Sound x))   -> go (Process (b_input Seq.|> x) b_ip b_mem, fb) (pa', fa) (fa cnt)
            (pa', Just (Recover x)) -> go (pb, fb) (pa', fa) cnt

    suspended m@(Process _ ip _) = let (Process _ ip' _, _) = eval (program Vector.! ip) m
                                   in ip == ip'
    processA = Process Seq.empty 0 Map.empty
    processB = Process Seq.empty 0 (Map.singleton 'p' 1)

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print (partOne program)
    print (partTwo program)
