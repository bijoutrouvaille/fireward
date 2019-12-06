module Combinators (
  _alpha,
  _alphaNum,
  _varStart,
  _varName,
  separated,
  escape,
  _getWhile,
  _stringD,
  _string,
  readDef,
  _natural,
  _float,
  void,
  altr,
  _terminated,
  _concat
) where

import Parser
import Control.Applicative (optional, empty)
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Data.List (intercalate)

_concat :: [Parser String] -> String -> Parser String
_concat ps sep = intercalate sep <$> sequence ps

_alpha = lower <|> upper
_alphaNum = _alpha <|> digit
_varStart = _alpha <|> charIn "_$"
_varName :: Parser String
_varName = do
  c <- _varStart
  rest <- many (_alphaNum <|> charIn "_$")
  return (c:rest)

separated = manywith . symbol

escape :: Char -> Parser String
escape c = do
  char '\\'
  r <- sat (==c)
  return ('\\':[r])

altr :: [Parser a] -> Parser a
altr [] = empty
altr (p:ps) = p <|> altr ps

_getWhile p = many $ sat p


_stringD :: Char -> Parser String
_stringD delim = do
  char delim
  a <- many $ (_const ('\\':[delim]) <|> (:[]) <$> sat (/=delim))
  require "unterminated string" $ char delim
  return $ concat (([delim]:a) ++ [[delim]])

_string :: Parser String
_string = _stringD '"' <|> _stringD '\''

readDef :: (Read a) => a -> String -> a
readDef def s = case reads s of
              [(x, "")] -> x
              _ -> def

_natural :: Parser Int
_natural = do  -- a natural number
  str <- some digit
  let n = readDef (-1) str
  guardWith "expected an integer" (n /= -1)
  return n

void :: Parser a -> Parser ()
void p = p >> return ()


_terminated parser terminator = do
  v <- parser
  terminator
  return v

toFloat :: Int -> Float
toFloat = fromIntegral -- x = x * 1.0 :: Double

_float :: Parser Float
_float = do
  neg <- optional $ symbol "-"
  optional space
  left <- toFloat <$> _natural 
  decMaybe <- optional _dec
  let decStr = maybe "0" id decMaybe :: String
  let dec = readDef 0 decStr :: Float
  let right = shiftDec dec
  return $ (left + right) * (if neg==Nothing then 1 else -1)
  where 
        shiftDec x = if x >= 1 then shiftDec (x/10) else x
        readRight x = (readDef 0 x) / (10.0 ^ (length x)) :: Double
        _dec = do
          symbol "."
          some digit
