module Main (main) where

import           Data.Bits (popCount, xor)
import           Data.Char
import           Data.List

type HashValues = ([Int], Int, Int)

main :: IO ()
main = print . countHex . generateGrid $ "ffayrhll"

countHex :: String -> Int
countHex = sum . map (popCount . fromHex)
  where
    fromHex x
      | x `elem` ['0'..'9'] = ord x - ord '0'
      | x `elem` ['a'..'f'] = ord x - ord 'a' + 10

generateGrid :: String -> String
generateGrid = concatMap hash . generateInput

generateInput :: String -> [String]
generateInput str = map (\d -> str ++ "-" ++ show d) [0..127]

-- From Day 10

hash :: String -> String
hash = toHexStr . denseHash . runRounds . parseInput

toHexStr :: [Int] -> String
toHexStr = concatMap $ \num -> (if num < 16 then "0" else "") ++ hexS num
  where
    hexS n = let (d, r) = n `divMod` 16
             in (if d == 0 then "" else hexS d) ++ [hex r]
    hex c
      | 0 <= c && c <= 9 = chr (c + ord '0')
      | otherwise = chr (c - 10 + ord 'a')

denseHash :: [Int] -> [Int]
denseHash = map (foldr1 xor) . groupN 16

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n l  = let (f, s) = splitAt n l in f : groupN n s

runRounds :: [Int] -> [Int]
runRounds l = (\(r, _, _) -> r) . (!! 64) $ iterate (\x -> foldl' tie x l) ([0..255], 0, 0)

tie :: HashValues -> Int -> HashValues
tie (inp, cp, skip) l = (rotate, (cp + l + skip) `mod` length inp, skip + 1)
  where
    rotate = let li = length inp
                 inf = cycle inp
                 end = reverse . take l . drop cp $ inf
                 le = length end
                 start = take (li - le) . drop (cp + le) $ inf
             in take li . drop (li - cp) $ cycle (end ++ start)

parseInput :: String -> [Int]
parseInput = (++ [17, 31, 73, 47, 23]) . map ord
