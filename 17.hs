{-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as Seq
import Data.List (foldl')

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
partTwo step = snd (foldl' go (0, 0) [1..50000000])
  where
    go (!pos, !val) x = (pos', val')
      where
        pos' = (pos + step) `rem` x + 1
        val' = if pos' == 1 then x else val
