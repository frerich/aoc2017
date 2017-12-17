{-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as Seq

main :: IO ()
main = do
    let input = 366
    print (partOne input)
    print (partTwo input)

partOne :: Int -> Int
partOne step = let (xs, pos) = foldl go (Seq.singleton 0, 0) [1..2017]
                in xs `Seq.index` (pos + 1)
  where
    go (xs, pos) v = let pos' = (pos + step) `rem` v + 1
                      in (insertAt pos' v xs, pos')

    insertAt i x xs = let (as, bs) = Seq.splitAt i xs in as Seq.>< x Seq.<| bs

partTwo :: Int -> Int
partTwo step = go (0, 0, 0) 1
  where
    go (_,    _,        !valueAfterNull) 50000000  = valueAfterNull
    go (!pos, !nullPos, !valueAfterNull) value     = go (pos', nullPos', valueAfterNull') (value + 1)
      where
        pos' = (pos + step) `rem` value + 1
        nullPos' = if pos' <= nullPos then nullPos + 1 else nullPos
        valueAfterNull' = if pos' == nullPos + 1 then value else valueAfterNull

