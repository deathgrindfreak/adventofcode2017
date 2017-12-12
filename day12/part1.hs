module Main (main) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

type AdjList = M.Map Int [Int]

main :: IO ()
main = length . S.toList . dfs . parseInput <$> readFile "input" >>= print

dfs :: AdjList -> S.Set Int
dfs adj = runDFS 0 (S.empty)
  where
    runDFS n mk =
      let marked = S.insert n mk
      in case filter (`S.notMember` marked) (adj M.! n) of
        [] -> marked
        xs  -> foldr (\x mkd -> runDFS x mkd) marked xs

parseInput :: String -> AdjList
parseInput = M.fromList . map (fst . head . readP_to_S parseLine) . lines

parseLine :: ReadP (Int, [Int])
parseLine = do
  n <- number
  _ <- string " <-> "
  a <- sepBy1 number (string ", ")
  eof
  return (n, a)
  where
    number = read <$> munch1 (`elem` ['0'..'9'])
