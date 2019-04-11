-- {-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

-- adapted from Monadic Parsing in Haskell
-- http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
-- and Thinking Functionally in Haskell


module Parser 
( getc
, sat
, satS
, none
, many
, alt
, enum
, some
, char
, string
, _const
, digit
, lower
, upper
, space
, optional'
, token
, symbol
, grouped
, manywith
, somewith
, oneOf
, apply
, parse
, Parser
, guard
, (<|>), (>>=)
, trim
) where 

import Data.Char ( isDigit , isLower , isUpper, isSpace)
import Data.List (dropWhileEnd)

import Control.Monad ((>>=), return, ap, liftM, guard)
import Control.Applicative (Alternative, (<|>), empty, many, some, optional)
import Prelude hiding (head)

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

satS :: (Char -> Bool) -> Parser String
satS p = (:[]) <$> sat p 

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser ()
string "" = return ()
string (c:cs) = do { char c; string cs; return () }


digit = sat isDigit
lower = sat isLower
upper = sat isUpper

head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = Just x

optional' :: Parser [a] -> Parser [a]
optional' p = p <|> none

lineComment = char '/' >> char '/' >> many (sat (\c->c/='\n' && c/='\r')) >> do
  return ()

spaceSat = sat isSpace >> return ()
space = many (spaceSat <|> lineComment ) >> return ()

token :: Parser a -> Parser a
token p = space >> p

symbol :: String -> Parser ()
symbol = token . string

grouped :: String -> String -> Parser a -> Parser a
grouped o c p = do { symbol o;
                     v <- p;
                     symbol c;
                     return v; }

manywith sep p = optional' (somewith sep p)
somewith sep p = do
  first <- p
  rest <- many (sep >> p)
  return (first:rest)

oneOf :: String -> Parser Char
oneOf "" = empty
oneOf (c:cs) = char c <|> oneOf cs

_const :: String -> Parser String
_const s = do
  string s
  return s
  
alt :: (a -> Parser a) -> [a] -> Parser a
alt p [] = fail ""
alt p (x:xs) = p x <|> alt p xs

enum :: [String] -> Parser String
enum xs = alt (\s -> do { symbol s ; return s }) xs

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

