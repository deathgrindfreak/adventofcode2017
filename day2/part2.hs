module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = checksum . parseLines <$> readFile "input" >>= print

parseLines :: String -> [[Int]]
parseLines = map (map read . words) . lines

checksum :: [[Int]] -> Int
checksum = sum . map (findDivs . sortBy (flip compare))
  where
    findDivs (x:xs) = case mapMaybe (divides x) xs of
      [] -> findDivs xs
      [y] -> y
    divides x y = if x `mod` y == 0 then Just (x `div` y) else Nothing
