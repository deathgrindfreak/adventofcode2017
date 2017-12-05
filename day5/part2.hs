module Main (main) where

import Data.Array.IArray

main :: IO ()
main = performJumps 0 0 . parseLines <$> readFile "input" >>= print

parseLines :: String -> Array Int Int
parseLines = (\x -> listArray (0, length x - 1) x) . map read . lines

performJumps :: Int -> Int -> Array Int Int -> Int
performJumps i nj a
  | i > (snd . bounds $ a) = nj
  | otherwise = let j = a ! i
                    o = if j >= 3 then -1 else 1
                in performJumps (i + j) (nj + 1) (a // [(i, j + o)])
