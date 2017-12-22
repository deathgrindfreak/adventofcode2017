module Main (main) where

import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP

data Dir = Up | Down deriving (Eq, Show)

data Layer = Layer
  { range :: Int
  , position :: Int
  , dir :: Dir
  } deriving Show

type FireWall = M.Map Int Layer

main :: IO ()
main = trip . parseInput <$> readFile "input" >>= print

trip :: FireWall -> Int
trip fw = (\(_, _, s) -> s) . (!! ((+ 2) . fst . M.findMax $ fw)) $ iterate tick (0, fw, 0)

tick :: (Int, FireWall, Int) -> (Int, FireWall, Int)
tick (cd, fw, score) = (cd + 1, moveScanners fw, newScore)
  where
    newScore = maybe score (\(Layer r p _) -> score + if p == 0 then cd * r else 0) (M.lookup cd fw)

moveScanners :: FireWall -> FireWall
moveScanners fw = foldr (M.adjust move) fw (M.keys fw)
  where
    move (Layer r p d)
      | r == 1 = Layer r p d
      | p == 0 || (p < r - 1 && d == Down) = Layer r (p + 1) Down
      | r - 1 == p || (p > 0 && d == Up)= Layer r (p - 1) Up
      | otherwise = error "Improper state"

parseInput :: String -> FireWall
parseInput = M.fromList . map (toLayer . fst . head . readP_to_S parseLine) . lines
  where toLayer (d, r) =  (d, Layer r 0 Down)

parseLine :: ReadP (Int, Int)
parseLine = number >>= \d -> string ": " >> number >>= \r -> eof >> return (d, r)
  where number = read <$> munch1 (`elem` ['0'..'9'])
