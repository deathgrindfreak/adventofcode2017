module Main (main) where

import           Control.Applicative
import           Data.List
import           Data.Maybe
import qualified Data.Sequence                as S
import           Text.ParserCombinators.ReadP

type Programs = S.Seq Char

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char
          deriving Show

main :: IO ()
main = dance (S.fromList ['a'..'p']) . parseDance <$> readFile "input" >>= print

dance :: Programs -> [Move] -> Programs
dance = foldl' danceMove
  where
    danceMove pr mv = case mv of
      Spin n       -> spin n pr
      Exchange l m -> exchange l m pr
      Partner o p  -> partner o p pr

spin :: Int -> Programs -> Programs
spin n pr = let (f, s) = S.splitAt (length pr - n) pr in s S.>< f

exchange :: Int -> Int -> Programs -> Programs
exchange li mi pr = let l = pr `S.index` li
                        m = pr `S.index` mi
                    in S.update mi l $ S.update li m pr

partner :: Char -> Char -> Programs -> Programs
partner o q pr = let oi = fromJust . S.elemIndexL o $ pr
                     qi = fromJust . S.elemIndexL q $ pr
                 in S.update qi o $ S.update oi q pr

parseDance :: String -> [Move]
parseDance = fst . head . readP_to_S parseMove

parseMove :: ReadP [Move]
parseMove = do
  ls <- sepBy1 (pSpin <|> pExchange <|> pPartner) (char ',')
  skipSpaces
  eof
  return ls
  where
    pSpin = char 's' >> number >>= return . Spin
    pExchange = char 'x' >> number >>= \s -> char '/' >> number >>= \e -> return $ Exchange s e
    pPartner = char 'p' >> get >>= \s -> char '/' >> get >>= \e -> return $ Partner s e
    number = read <$> munch1 (`elem` ['0'..'9'])
