-- {-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

-- adapted from Monadic Parsing in Haskell
-- http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
-- and Thinking Functionally in Haskell


module Parser 
( getc
, sat
, none
, char
, digit
, lower
, upper
, space
, optional'
, token
, symbol
, grouped
, apply
, parse
) where 

import Data.Char ( isDigit , isLower , isUpper, isSpace)

import Control.Monad ((>>=), return, ap, liftM, guard)
import Control.Applicative (Alternative, (<|>), empty, many)

newtype Parser a = Parser (String -> [(a, String)])


maybeHead [] = Nothing
maybeHead (x:_) = Just x

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> Maybe a
parse p = fmap fst . maybeHead . apply p

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

none :: Parser [a]
none = return []

instance Alternative Parser where
  empty = Parser (\s -> [])
  p <|> q = Parser f where
    f s = let ps = apply p s in
              if null ps then apply q s 
                         else ps

instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  p >>= q = Parser (\s -> [ (y, s'')
                          | (x, s') <- apply p s,
                            (y, s'') <- apply (q x) s' ] )


getc :: Parser Char
getc = Parser f where
  f [] = []
  f (c:cs) = [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- getc; guard (p c); return c }

char :: Char -> Parser ()
char x = do { c <- sat (==x); return () }

string :: String -> Parser ()
string "" = return ()
string (c:cs) = do { char c; string cs; return () }

digit = sat isDigit
lower = sat isLower
upper = sat isUpper

optional' :: Parser [a] -> Parser [a]
optional' p = p <|> none

space = many (sat isSpace) >> return ()

token :: Parser a -> Parser a
token p = space >> p

symbol :: String -> Parser ()
symbol = token . string

grouped :: String -> String -> Parser a -> Parser a
grouped o c p = do { symbol o;
                     v <- p;
                     symbol c;
                     return v; }

