module Main where

main :: IO ()
main = checksum . parseLines <$> readFile "input" >>= print

parseLines :: String -> [[Int]]
parseLines = map (map read . words) . lines

checksum :: [[Int]] -> Int
checksum = sum . map (\x -> maximum x - minimum x)
