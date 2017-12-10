module Main (main) where

import Data.List

data ProcessState = Begin
                  | Group
                  | Garbage
                  | Ignore
                  deriving Show

data GroupState = GroupState
  { state :: [ProcessState],
    level :: Int,
    score :: Int
  } deriving Show

main :: IO ()
main = score . process . init <$> readFile "input" >>= print

process :: String -> GroupState
process = foldl' processChar (GroupState [Begin] 0 0)
  where
    processChar (GroupState [] _ _) c = error $ "Empty state stack: " ++ show c
    processChar (GroupState (s:ss) l sc) c = case s of
      Begin  -> GroupState (Group : ss) 1 1
      Ignore -> GroupState ss l sc
      _      -> nextState (s:ss) l sc c

    nextState [] _ _ _ = error "Empty state stack"
    nextState (s:ss) l sc c = case s of
      Garbage -> case c of
        '!' -> GroupState (Ignore : s : ss) l sc
        '>' -> GroupState ss l sc
        _   -> GroupState (s:ss) l sc
      Group -> case c of
        '}' -> GroupState ss (l - 1) sc
        '<' -> GroupState (Garbage : s : ss) l sc
        '{' -> GroupState (Group : s : ss) (l + 1) (sc + l + 1)
        ',' -> GroupState (s:ss) l sc
        _   -> error $ "Malformed group: " ++ show c
      _ -> error $ "Improper state: " ++ show s
