module Main (main) where

import           Data.List
import           Text.ParserCombinators.ReadP

main :: IO ()
main = product . take 2 . hash . parseInput <$> readFile "input" >>= print

hash :: [Int] -> [Int]
hash = (\(r, _, _) -> r) . foldl' tie ([0..255], 0, 0)

tie :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
tie (inp, cp, skip) l = (rotate, (cp + l + skip) `mod` length inp, skip + 1)
  where
    rotate = let li = length inp
                 inf = cycle inp
                 end = reverse . take l . drop cp $ inf
                 le = length end
                 start = take (li - le) . drop (cp + le) $ inf
             in take li . drop (li - cp) $ cycle (end ++ start)

parseInput :: String -> [Int]
parseInput = fst . head . readP_to_S parseList

parseList :: ReadP [Int]
parseList = sepBy1 number (char ',') >>= \l -> skipSpaces >> eof >> return l
  where
    number = read <$> munch1 (`elem` ['0'..'9'])
