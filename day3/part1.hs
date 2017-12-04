module Main where

main :: IO ()
main = print $ manhattanDist 312051

manhattanDist :: Int -> Int
manhattanDist n = last $ take n distPattern

-- Prints the manhattan distance pattern for the spiral grid
distPattern :: [Int]
distPattern = concat $ [0] : dist 1
  where
    dist n = replicate 4 (side n) ++ dist (n + 2)
    side n = [n,(n-1)..((n+1) `div` 2)] ++ [((n+1) `div` 2 + 1)..(n+1)]
