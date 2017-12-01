module Main where

import Data.Array.IArray
import Data.Char
import Data.Maybe

main :: IO ()
main = init `fmap` readFile "input" >>= print . halfWayPairs

halfWayPairs :: String -> Int
halfWayPairs l = sum . mapMaybe fp $ [0..(length l - 1)]
  where
    h = length l `div` 2

    m :: Array Int Int
    m = listArray (0, length l - 1) . map (\c -> ord c - ord '0') $ l

    fp i = if x == y then Just x else Nothing
      where
        x = m ! i
        y = m ! ((i + h) `mod` length l)
