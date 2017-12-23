import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List (unfoldr)
import           Text.Parsec hiding (count)

data Operand
    = Register Char
    | Number Int
    deriving Show

data Instruction
    = Set Char Operand
    | Sub Char Operand
    | Mul Char Operand
    | Jnz Operand Operand
    deriving Show

type Program = Vector Instruction
type Memory = Map Char Int
data Process = Process Program Int Memory deriving Show


-- | Parse a string into a program, a sequence of instructions
parseProgram :: String -> Program
parseProgram = either (error . show) id . parse program ""
  where
    program = Vector.fromList <$> instruction `endBy` newline
    instruction = try set <|> sub <|> mul <|> jnz
    set = Set <$> (string "set " *> lower <* space) <*> operand
    sub = Sub <$> (string "sub " *> lower <* space) <*> operand
    mul = Mul <$> (string "mul " *> lower <* space) <*> operand
    jnz = Jnz <$> (string "jnz " *> operand <* space) <*> operand
    operand = register <|> number
    register = Register <$> lower
    number = Number <$> int
    int = do
        sign <- option id (char '-' >> return negate)
        decimal <- read <$> many1 digit
        return (sign decimal)


-- | Execute a single instruction in the given process, returning the new process
exec :: Instruction -> Process -> Process
exec instr (Process input ip mem) =
    case instr of
        Set x y -> Process input (ip + 1) (Map.insert x (value y) mem)
        Sub x y -> Process input (ip + 1) (Map.insert x (value (Register x) - value y) mem)
        Mul x y -> Process input (ip + 1) (Map.insert x (value (Register x) * value y) mem)
        Jnz x y -> if value x /= 0
                    then Process input (ip + value y) mem
                    else Process input (ip + 1) mem
  where
    value (Register r) = Map.findWithDefault 0 r mem
    value (Number x)   = x


-- | Execute the given program until completion and return the number of multiplications
partOne :: Program -> Int
partOne prog = count isMul (unfoldr step (Process prog 0 Map.empty))
  where
    step p@(Process prog ip _)
        | ip < 0 || ip >= Vector.length prog = Nothing
        | otherwise                         = Just (prog Vector.! ip, exec (prog Vector.! ip) p)

    isMul (Mul _ _) = True
    isMul _         = False


-- | Yield the number of elements in a given sequence matching the given condition
count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)


main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print (partOne program)
