module Parse.NumParse (parseNumber) where

import Lisp

import Control.Monad
import Numeric (readDec, readOct, readHex, readFloat)
import Text.ParserCombinators.Parsec hiding (spaces)

readBin :: String -> [(Integer, String)]
readBin s = let
  (leftOver, usable) = break (\x -> x == '0' || x == '1') s
  numList = map (read . (:[])) usable
  placeList = reverse [0..length s - 1]
  in [(sum $ zipWith (\x n -> x * 2^n) numList placeList, leftOver)]

parseFloat :: Parser LispVal
parseFloat = try $ do
  begin <- many1 digit
  c     <- char '.'
  rest  <- many1 digit
  (return . Real . fst . head . readFloat) (begin ++ (c : rest))

parseBase :: Parser LispVal
parseBase = try $ do
  char '#'
  base <- oneOf "bodx"
  rest <- many digit
  unusable <- many anyChar
  if unusable /= "" then fail $ "Non-digit " ++ unusable ++ " in Integer" else
    helper base rest
  where
    helper' :: [(Integer, String)] -> Parser LispVal
    helper' = return . Integer . fst . head
    helper :: Char -> String -> Parser LispVal
    helper 'b' s = helper' $ readBin s
    helper 'd' s = helper' $ readDec s
    helper 'o' s = helper' $ readOct s
    helper 'x' s = helper' $ readHex s
    helper  b  s = fail $ "This is a bug with input " ++ ('#' : b : s)


parseInt :: Parser LispVal
parseInt = liftM (Integer . read) $ many1 digit

parseNumber :: Parser LispVal
parseNumber =  parseFloat <|> parseBase <|> parseInt
