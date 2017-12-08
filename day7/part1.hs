module Main (main) where

import Data.List
import Text.ParserCombinators.ReadP

data ProgTree = Node
  { name :: String
  , weight :: Int
  , children :: [ProgTree]
  } deriving (Show, Eq)

main :: IO ()
main = name . buildTree . parseInput <$> readFile "input" >>= print

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
