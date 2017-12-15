module Main (main) where

import           Data.Char
import qualified Data.List                    as L
import qualified Data.Map.Strict              as M
import           Text.ParserCombinators.ReadP

data Dir = N | S | NE | NW | SE | SW
  deriving (Read, Show, Eq, Ord)

type Path = M.Map Dir Int

main :: IO ()
main = walk . parseInput <$> readFile "input" >>= print

walk :: [Dir] -> Int
walk = maximum . map (sumAll . M.toList) . L.scanl' walkDir M.empty
  where
    walkDir path = combine . initReduce . addToPath path
    addToPath path step = let v = maybe 0 id (M.lookup step path)
                          in M.insert step (v + 1) path

sumAll :: [(a, Int)] -> Int
sumAll = foldr (\x a -> a + snd x) 0

combine :: Path -> Path
combine ls = foldr (\(m, n) a -> combineStep a m n) ls pairs
  where
    pairs = [(SE, SW), (NE, NW), (S, NW), (S, NE), (N, SW), (N, SE)]

    combineStep mp m n = case (M.lookup m mp, M.lookup n mp) of
      (Just ml, Just nl) -> handleCombine ml nl
      _                  -> mp
      where
        handleCombine ml nl = foldr ($) mp cmb
          where
            ndir = comb m n
            ncnt = maybe 0 id (M.lookup ndir mp)
            cmb
              | ml == nl = [M.insert ndir (ncnt + ml), M.delete n, M.delete m]
              | ml < nl = [M.insert ndir (ncnt + ml), M.insert n (nl - ml), M.delete m]
              | otherwise = [M.insert ndir (ncnt + nl), M.insert m (ml - nl), M.delete n]

    comb SE SW = S
    comb NE NW = N
    comb S NW  = SW
    comb S NE  = SE
    comb N SW  = NW
    comb N SE  = NE

initReduce :: Path -> Path
initReduce ls = foldr (\(m, n) a -> reduceStep a m n) ls path
  where
    path = [(S, N), (NE, SW), (NW, SE)]
    reduceStep l m n = case (M.lookup m l, M.lookup n l) of
      (Just ml, Just nl) -> let big = if ml > nl then m else n
                                small = if ml > nl then n else m
                            in M.insert big (abs $ ml - nl) (M.delete small l)
      _ -> l

parseInput :: String -> [Dir]
parseInput = map (read . map toUpper) . fst . head . readP_to_S parseList

parseList :: ReadP [String]
parseList = sepBy1 dir (char ',') >>= \l -> skipSpaces >> eof >> return l
  where dir = munch1 (`elem` "nesw")
