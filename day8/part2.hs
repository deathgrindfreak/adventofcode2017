module Main (main) where

import Control.Applicative
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import Data.Ord
import Text.ParserCombinators.ReadP

type Input = (String, String, Int, String, String, Int)
type Registers = M.Map String Int

data Op = Op
  { targetReg :: String
  , condReg :: String
  , op :: Int -> Int -> Int
  }

main :: IO ()
main = fst . runOps . parseInput <$> readFile "input" >>= print

maxVal :: Registers -> Int
maxVal = snd . maximumBy (comparing snd) . M.toList

runOps :: [Op] -> (Int, Registers)
runOps = foldl' runOp (0, M.empty)
  where
    runOp (mx, m) (Op t c opn) =
      let tr = fromMaybe 0 (M.lookup t m)
          cr = fromMaybe 0 (M.lookup c m)
          nm = M.insert t (opn tr cr) (M.insert c cr m)
          mapMax = maxVal nm
      in (max mx mapMax, nm)

parseInput :: String -> [Op]
parseInput = map (fst . head . readP_to_S (toOp parseLine)) . lines

toOp :: ReadP Input -> ReadP Op
toOp ip = ip >>= \(tr, top, tv, cr, cop, cv) ->
  return $ Op tr cr (getOp top tv cop cv)
  where
    getOp top tv cop cv = \newTv newCv ->
      if (opMap M.! cop) newCv cv
      then getTOp top tv newTv
      else newTv

    getTOp o v = if o == "inc" then (+ v) else (subtract v)

    opMap = M.fromList $
      [ ("<",  (<))
      , ("<=", (<=))
      , (">",  (>))
      , (">=", (>=))
      , ("==", (==))
      , ("!=", (/=))
      ]

parseLine :: ReadP Input
parseLine = do
  r <- asciiStr
  skipSpaces
  o <- string "inc" <|> string "dec"
  skipSpaces
  amt <- number
  _ <- string " if "
  cr <- asciiStr
  skipSpaces
  cop <- operation
  skipSpaces
  cov <- number
  eof
  return (r, o, amt, cr, cop, cov)
  where
    asciiStr = munch1 (`elem` ['a'..'z'])

    number = do
      sign <- option "" (string "-")
      n <- munch1 (`elem` ['0'..'9'])
      return $ read (sign ++ n)

    operation = munch1 (`elem` "<>=!")
