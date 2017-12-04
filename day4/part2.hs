module Main where

import Data.List (nub, sort)

main :: IO ()
main = foldr ((\x a -> a + if x then 1 else 0) . isDistinct . words) 0 . lines <$> readFile "input" >>= print

isDistinct :: (Eq a, Ord a) => [[a]] -> Bool
isDistinct x = (length . nub . map sort $ x) == length x
