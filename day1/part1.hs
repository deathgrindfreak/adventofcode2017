module Main where

import Data.Char

main :: IO ()
main = init `fmap` readFile "input" >>= print . sumPairs

sumPairs :: String -> Int
sumPairs = foldr (\x a -> a + ord x - ord '0') 0 . findPairs

findPairs :: Eq a => [a] -> [a]
findPairs l = fp (last l : l)
  where
    fp [] = []
    fp [_] = []
    fp (x:y:xs)
      | x == y = x : fp (y:xs)
      | otherwise = fp (y:xs)
