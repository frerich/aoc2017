import qualified Data.Vector.Unboxed as V
import Data.List (unfoldr)
import Data.Maybe (catMaybes, isJust)

type Memory = (Int, V.Vector Int)

parse :: String -> Memory
parse s = (0, V.fromList . map read . lines $ s)

step :: (Int -> Int) -> Memory -> Memory
step adjust (pos, mem) =
    if val < 0 && pos + val >= 0 || val >= 0 && pos + val < V.length mem
      then (pos + val, mem V.// [(pos, adjust val)])
      else ((-1), V.empty)
  where
    val = mem V.! pos

partOne :: String -> Int
partOne = length . takeWhile (not . V.null . snd) . iterate (step (+1)) . parse

partTwo :: String -> Int
partTwo = length . takeWhile (not . V.null . snd) . iterate (step (\x -> if x >= 3 then x - 1 else x + 1)) . parse

main :: IO ()
main = interact $ show . partOne . parse
