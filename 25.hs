import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Text.Parsec

data Movement = GoLeft | GoRight deriving (Eq, Show)
type StateName = Char
type Transition = (Int, Movement, StateName)
data Blueprint = Blueprint StateName Int (Map StateName (Transition, Transition)) deriving Show
data Machine = Machine (IntMap Int) StateName Int


main :: IO ()
main = do
    blueprint <- parseBlueprint <$> getContents
    print (partOne blueprint)


parseBlueprint :: String -> Blueprint
parseBlueprint = either (error . show) id . parse blueprint ""
  where
    blueprint    = Blueprint <$> initialState <*> numSteps <*> states
    initialState = upper `after` "Begin in state "
    numSteps     = decimal `after` "Perform a diagnostic checksum after "
    states       = newline *> (Map.fromList <$> state `sepBy` newline)

    state = do
        name <- upper `after` "In state "
        if0 <- transition
        if1 <- transition
        return (name, (if0, if1))

    transition = do
      spaces >> string "If the current value is" >> toEOL
      (,,) <$> (decimal `after` "- Write the value ")
           <*> (movement `after` "- Move one slot to the ")
           <*> (upper `after` "- Continue with state ")

    movement = (string "left" >> return GoLeft)
           <|> (string "right" >> return GoRight)

    after p str = spaces >> string str *> p <* toEOL
    toEOL       = anyChar `manyTill` newline >> return ()
    decimal     = read <$> many1 digit


-- | Get the checksum of building and running a machine according to the blue print
partOne :: Blueprint -> Int
partOne (Blueprint initialState numSteps states) = checksum (iterate (step states) machine !! numSteps)
  where
    machine = Machine IntMap.empty initialState 0


-- | Get the current checksum of a machine (the number of all 1s on the tap)
checksum :: Machine -> Int
checksum (Machine tape _ _) = sum (IntMap.elems tape)


-- | Execute a single step in a turing machine according to the given state table
step :: Map StateName (Transition, Transition) -> Machine -> Machine
step states (Machine tape state pos) = Machine tape' state' pos'
    where
        (t0, t1) = states Map.! state
        (value, movement, state')
            | IntMap.findWithDefault 0 pos tape == 0 = t0
            | otherwise                             = t1
        tape' = IntMap.insert pos value tape
        pos'
            | movement == GoLeft = pos - 1
            | otherwise         = pos + 1
