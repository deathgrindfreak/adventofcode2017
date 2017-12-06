module Main (main) where

import qualified Data.IntMap as M
import qualified Data.Map.Strict as Map
import qualified Data.Set as S

type Banks = M.IntMap Int

main :: IO ()
main = loopSize . parseInput <$> readFile "input" >>= print
  where
    parseInput :: String -> Banks
    parseInput = M.fromList . zip [0..] . map read . words

loopSize :: Banks -> Int
loopSize ms = num 1 (S.singleton ms) (Map.singleton ms 1) ms
  where
    num i s mp m = let r = redistribute m
                   in if S.member r s
                      then i - (mp Map.! r)
                      else num (i + 1) (S.insert r s) (Map.insert r i mp) r

redistribute :: Banks -> Banks
redistribute ms = red ((x + 1) `mod` M.size ms) y (M.insert x 0 ms)
  where
    (x, y) = maxElem (M.toList ms)

    maxElem (t:ts) = mx t ts
      where
        mx l [] = l
        mx l@(_, lp) (z@(_, w) : ys)
          | lp >= w = mx l ys
          | otherwise = mx z ys

    red i n m
      | n == 0 = m
      | otherwise = red ((i + 1) `mod` M.size m) (n - 1) (M.insert i ((m M.! i) + 1) m)
