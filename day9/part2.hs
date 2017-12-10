module Main (main) where

import Data.List

data ProcessState = Begin
                  | Group
                  | Garbage
                  | Ignore
                  deriving Show

data GroupState = GroupState
  { state :: [ProcessState],
    totalGarbage :: Int
  } deriving Show

main :: IO ()
main = totalGarbage . process . init <$> readFile "input" >>= print

process :: String -> GroupState
process = foldl' processChar (GroupState [Begin] 0)
  where
    processChar (GroupState [] _) c = error $ "Empty state stack: " ++ show c
    processChar (GroupState (s:ss) tg) c = case s of
      Begin  -> GroupState (Group : ss) tg
      Ignore -> GroupState ss tg
      _      -> nextState (s:ss) tg c

    nextState [] _ _ = error "Empty state stack"
    nextState (s:ss) tg c = case s of
      Garbage -> case c of
        '!' -> GroupState (Ignore : s : ss) tg
        '>' -> GroupState ss tg
        _   -> GroupState (s:ss) (tg + 1)
      Group -> case c of
        '}' -> GroupState ss tg
        '<' -> GroupState (Garbage : s : ss) tg
        '{' -> GroupState (Group : s : ss) tg
        ',' -> GroupState (s:ss) tg
        _   -> error $ "Malformed group: " ++ show c
      _ -> error $ "Improper state: " ++ show s
