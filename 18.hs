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
data Machine = Machine Queue Int Memory

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

eval :: Instruction -> (Char -> Machine -> (Machine, Maybe SoundAction)) -> Machine -> (Machine, Maybe SoundAction)
eval instr rcvHandler (Machine input ip mem) =
    case instr of
        Snd x   -> (Machine input (ip + 1) mem, Just (Sound (value x)))
        Set x y -> (Machine input (ip + 1) (Map.insert x (value y)                          mem), Nothing)
        Add x y -> (Machine input (ip + 1) (Map.insert x (value (Register x) + value y)     mem), Nothing)
        Mul x y -> (Machine input (ip + 1) (Map.insert x (value (Register x) * value y)     mem), Nothing)
        Mod x y -> (Machine input (ip + 1) (Map.insert x (value (Register x) `rem` value y) mem), Nothing)
        Rcv x   -> rcvHandler x (Machine input ip mem)
        Jgz x y -> if value x > 0
                    then (Machine input (ip + value y) mem, Nothing)
                    else (Machine input (ip + 1) mem, Nothing)
  where
    value (Register r) = Map.findWithDefault 0 r mem
    value (Number x)   = x

partOne :: Program -> Int
partOne program = go (Machine Seq.empty 0 Map.empty)
  where
    go machine@(Machine input ip mem) =
        case eval (program Vector.! ip) rcvHandler machine of
            (machine'               , Nothing)          -> go machine'
            (Machine input' ip' mem', Just (Sound x))   -> go (Machine (input Seq.|> x) ip' mem')
            (_                      , Just (Recover x)) -> x

    rcvHandler x (Machine input ip mem) =
        if Map.findWithDefault 0 x mem /= 0
            then (Machine input (ip + 1) mem, Just (Recover (Seq.index input (Seq.length input - 1))))
            else (Machine input (ip + 1) mem, Nothing)

partTwo :: Program -> Int
partTwo program = go (processA, id) (processB, (+1)) 0
  where
    go (pa@(Machine a_input a_ip a_mem), fa) (pb@(Machine b_input b_ip b_mem), fb) cnt
        | suspended pa && suspended pb = cnt
        | otherwise =
        case eval (program Vector.! a_ip) rcvHandler pa of
            (pa', Nothing)          -> go (pb, fb) (pa', fa) cnt
            (pa', Just (Sound x))   -> go (Machine (b_input Seq.|> x) b_ip b_mem, fb) (pa', fa) (fa cnt)
            (pa', Just (Recover x)) -> go (pb, fb) (pa', fa) cnt

    rcvHandler x (Machine input ip mem) =
        if Seq.null input
            then (Machine input ip mem, Nothing)
            else (Machine (Seq.drop 1 input) (ip + 1) (Map.insert x (Seq.index input 0) mem), Just (Recover 0))

    suspended :: Machine -> Bool
    suspended m@(Machine _ ip _) = let (Machine _ ip' _, _) = eval (program Vector.! ip) rcvHandler m
                                   in ip == ip'

    processA = Machine Seq.empty 0 Map.empty
    processB = Machine Seq.empty 0 (Map.singleton 'p' 1)

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print (partOne program)
    print (partTwo program)
