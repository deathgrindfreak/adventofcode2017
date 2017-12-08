module Main (main) where

import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

data ProgTree = Node
  { name :: String
  , weight :: Int
  , children :: [ProgTree]
  } deriving (Show, Eq)

main :: IO ()
main = findUnbalanced 0 . buildTree . parseInput <$> readFile "input" >>= print

findUnbalanced :: Int -> ProgTree -> Int
findUnbalanced diff t = case checkBalance t of
  Nothing -> weight t + diff
  Just (i, d) -> findUnbalanced d (children t !! i)

checkBalance :: ProgTree -> Maybe (Int, Int)
checkBalance t = if balanced chSums
                 then Nothing
                 else Just (i, snd unb)
  where
    i = fromJust $ elemIndex (fst unb) chSums

    chSums = map sumChildWts . children $ t

    unb = let mn = minimum chSums
              mx = maximum chSums
          in if ((length . filter (== mn) $ chSums) == 1)
             then (mn, mx - mn)
             else (mx, mn - mx)

    balanced [] = True
    balanced xs = minimum xs == maximum xs

sumChildWts :: ProgTree -> Int
sumChildWts = sum . sumWts
  where sumWts (Node _ w ch) = w : concatMap (sumWts) ch

buildTree :: [ProgTree] -> ProgTree
buildTree [] = error "Empty tree"
buildTree [x] = x
buildTree (x:xs) = buildTree $ case findChildren x xs of
  ([], _) -> xs ++ [x]
  (ch, rs) -> rs ++ [foldr addChild x ch]
  where
    getChildNames ch = map name ch ++ concatMap (getChildNames . children) ch
    findChildren (Node _ _ ch) = let c = getChildNames ch in partition (\n -> name n `elem` c)
    addChild child (Node cn cw ch)
      | cn == name child = child
      | otherwise = (Node cn cw (map (addChild child) ch))

parseInput :: String -> [ProgTree]
parseInput = map (fst . head . readP_to_S parseLine) . lines

parseLine :: ReadP ProgTree
parseLine = do
  n <- asciiStr
  skipSpaces
  w <- numberParens
  _ <- optional (string " -> ")
  c <- sepBy asciiStr (string ", ")
  eof
  return $ Node n w (map (\nm -> Node nm 0 []) c)
  where
    asciiStr = munch1 (`elem` ['a'..'z'])
    number = munch1 (`elem` ['0'..'9'])
    numberParens = between (char '(') (char ')') (read <$> number)
