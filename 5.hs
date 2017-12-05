import qualified Data.Vector.Unboxed as V
import Data.List (unfoldr)

data Memory = Memory Int (V.Vector Int) deriving Show

parse :: String -> Memory
parse s = Memory 0 (V.fromList . map read . lines $ s)

step :: (Int -> Int) -> Memory -> Maybe Memory
step adjust (Memory pos mem) =
    if val < 0 && pos + val >= 0 || val >= 0 && pos + val < V.length mem
      then Just $ Memory (pos + val) (mem V.// [(pos, adjust val)])
      else Nothing
  where
    val = mem V.! pos

walk :: (Int -> Int) -> Memory -> [Memory]
walk adjust m = m : unfoldr (\x -> case step adjust x of Just m' -> Just (m', m'); Nothing -> Nothing) m

partOne :: String -> Int
partOne = length . walk (+1) . parse

partTwo :: String -> Int
partTwo = length . walk (\x -> if x >= 3 then x - 1 else x + 1) . parse

main :: IO ()
main = interact $ show . partOne
