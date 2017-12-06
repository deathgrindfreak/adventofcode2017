module Main (main) where

import qualified Data.Map.Strict as M
import Data.Maybe

type Coord = (Int, Int)

main :: IO ()
main = print . head . dropWhile (< 312051) $ accumCoords

accumCoords :: [Int]
accumCoords = 1 : acc (0, 0) (M.fromList [((0, 0), 1)])
  where
    acc c m = s : acc n (M.insert n s m)
      where
        n = nextCoord c
        s = sum $ mapMaybe (`M.lookup` m) (adjCoords n)

adjCoords :: Coord -> [Coord]
adjCoords (x, y) = [(x + a, y + b) | a <- [-1..1], b <- [-1..1], a /= 0 || b /= 0]

-- Messy unfortunately ...
nextCoord :: Coord -> Coord
nextCoord (x, y)
  | x == (-y) && x >= 0 && y <= 0 = (x + 1, y)
  | x == y    && x < 0 && y < 0 = (x + 1, y)
  | x == y    && x > 0 && y > 0 = (x - 1, y)
  | x == (-y) && x < 0 && y > 0 = (x, y - 1)
  | abs y < abs x && x > 0 = (x, y + 1)
  | abs y > abs x && y > 0 = (x - 1, y)
  | abs y < abs x && x < 0 = (x, y - 1)
  | abs y > abs x && y < 0 = (x + 1, y)

