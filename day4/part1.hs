module Main where

import Data.List (nub)

main :: IO ()
main = foldr ((\x a -> a + if x then 1 else 0) . isDistinct . words) 0 . lines <$> readFile "input" >>= print

isDistinct :: Eq a => [a] -> Bool
isDistinct x = (length . nub $ x) == length x
