module Main (main) where

import           Data.Bits
import           Text.ParserCombinators.ReadP

factorA, factorB, divisor, bitComp :: Int
factorA = 16807
factorB = 48271
divisor = 2147483647
bitComp = 2 ^ 16 - 1

nComps :: Int
nComps = 5000000

main :: IO ()
main = matches . generate . (\(x:y:_) -> (x, y)) . parseInput <$> readFile "input" >>= print

matches :: [Bool] -> Int
matches = length . filter (== True) . take nComps

generate :: (Int, Int) -> [Bool]
generate (pa, pb) = zipWith lower16Match (generateSeq factorA divBy4 pa) (generateSeq factorB divBy8 pb)

lower16Match :: Int -> Int -> Bool
lower16Match a b = ((a .&. bitComp) `xor` (b .&. bitComp)) == 0

divBy4, divBy8 :: Int -> Bool
divBy4 v = v .&. 3 == 0
divBy8 v = v .&. 7 == 0

generateSeq :: Int -> (Int -> Bool) -> Int -> [Int]
generateSeq factor d p = let v = factor * p `mod` divisor
                         in if d v
                            then v : generateSeq factor d v
                            else generateSeq factor d v

parseInput :: String -> [Int]
parseInput = map (fst . head . readP_to_S parseLine) . lines

parseLine :: ReadP Int
parseLine = ascii >> number >>= \n -> eof >> return n
  where
    ascii = munch1 (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ [' '])
    number = read <$> munch1 (`elem` ['0'..'9'])
