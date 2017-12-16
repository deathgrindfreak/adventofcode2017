module Main (main) where

import           Data.Bits
import           Text.ParserCombinators.ReadP

factorA, factorB, divisor, bitComp :: Int
factorA = 16807
factorB = 48271
divisor = 2147483647
bitComp = 2 ^ 16 - 1

nComps :: Int
nComps = 40000000

main :: IO ()
main = generate nComps 0 . (\(x:y:_) -> (x, y)) . parseInput <$> readFile "input" >>= print

generate :: Int -> Int -> (Int, Int) -> Int
generate 0 s _ = s
generate n s (pa, pb) = generate (n - 1) (s + if lower16Match pa' pb' then 1 else 0) (pa', pb')
  where (pa', pb') = generateNext (pa, pb)

lower16Match :: Int -> Int -> Bool
lower16Match a b = ((a .&. bitComp) `xor` (b .&. bitComp)) == 0

generateNext :: (Int, Int) -> (Int, Int)
generateNext (pa, pb) = (factorA * pa `mod` divisor, factorB * pb `mod` divisor)

parseInput :: String -> [Int]
parseInput = map (fst . head . readP_to_S parseLine) . lines

parseLine :: ReadP Int
parseLine = ascii >> number >>= \n -> eof >> return n
  where
    ascii = munch1 (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ [' '])
    number = read <$> munch1 (`elem` ['0'..'9'])
