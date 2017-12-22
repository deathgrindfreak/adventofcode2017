{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Control.Applicative
import qualified Control.Monad.State          as ST
import qualified Data.Map.Strict              as M
import           Text.ParserCombinators.ReadP

data Opt = Reg Char | Val Int deriving Show

data Instr = Snd Char
           | Set Char Opt
           | Add Char Opt
           | Mul Char Opt
           | Mod Char Opt
           | Rcv Char
           | Jgz Opt Opt
           deriving Show

type Registers = M.Map Char Int
type RegisterState = ST.State (Int, [Int], [Instr]) (Either Int Registers)

main :: IO ()
main = execProgram . parseInput <$> readFile "input" >>= print

execProgram :: [Instr] -> Int
execProgram = ST.evalState (pr initRegs) . (0,[],)
  where
    initRegs = M.fromList $ map (,0) ['a'..'z']

    pr reg = program reg >>= \res ->
      case res of
        Right reg' -> pr reg'
        Left a     -> return a

program :: Registers -> RegisterState
program reg = do
  (pc, played, stack) <- ST.get
  case runInstr pc played stack of
    Right (pc', played', reg') -> do
      ST.put (pc', played', stack)
      return $ Right reg'
    Left a -> return $ Left a
  where
    runInstr pc pl st = case st !! pc of
      Snd v               -> Right (pc + 1, (reg M.! v) : pl, reg)
      Set r (Reg s)       -> Right (pc + 1, pl, M.insert r (reg M.! s) reg)
      Set r (Val i)       -> Right (pc + 1, pl, M.insert r i reg)
      Add r (Reg s)       -> setr r s (+)
      Add r (Val i)       -> setv r i (+)
      Mul r (Reg s)       -> setr r s (*)
      Mul r (Val i)       -> setv r i (*)
      Mod r (Reg s)       -> setr r s mod
      Mod r (Val i)       -> setv r i mod
      Rcv r               -> if reg M.! r == 0
                             then Right (pc + 1, pl, reg)
                             else Left (head pl)
      Jgz (Val i) (Val j) -> jumpv i j
      Jgz (Val i) (Reg s) -> jumpv i (reg M.! s)
      Jgz (Reg r) (Val j) -> jumpr r j
      Jgz (Reg r) (Reg s) -> jumpr r (reg M.! s)
      where
        setr r s op = Right (pc + 1, pl, M.insert r (op (reg M.! r) (reg M.! s)) reg)
        setv r i op = Right (pc + 1, pl, M.insert r (op (reg M.! r) i) reg)
        jumpr r s = Right (pc + if reg M.! r == 0 then 1 else s, pl, reg)
        jumpv r s = Right (pc + if r == 0 then 1 else s, pl, reg)

parseInput :: String -> [Instr]
parseInput = map (fst . head . readP_to_S parseLine) . lines

parseLine :: ReadP Instr
parseLine = sndP <|> setP <|> addP <|> mulP <|> modP <|> rcvP <|> jgzP
  where
    sndP = string "snd" >> skipSpaces >> ascii >>= \r -> return $ Snd r
    setP = string "set" >> skipSpaces >> ascii >>= \r -> skipSpaces >> opt >>= \i -> return $ Set r i
    addP = string "add" >> skipSpaces >> ascii >>= \r -> skipSpaces >> opt >>= \i -> return $ Add r i
    mulP = string "mul" >> skipSpaces >> ascii >>= \r -> skipSpaces >> opt >>= \i -> return $ Mul r i
    modP = string "mod" >> skipSpaces >> ascii >>= \r -> skipSpaces >> opt >>= \i -> return $ Mod r i
    rcvP = string "rcv" >> skipSpaces >> ascii >>= \r -> return $ Rcv r
    jgzP = string "jgz" >> skipSpaces >> opt >>= \r -> skipSpaces >> opt >>= \i -> return $ Jgz r i

    opt = (ascii >>= return . Reg) <|> (number >>= return . Val)
    ascii = satisfy (`elem` ['a'..'z'])
    number = do
      sign <- option "" (string "-")
      n <- munch1 (`elem` ['0'..'9'])
      return $ read (sign ++ n)
