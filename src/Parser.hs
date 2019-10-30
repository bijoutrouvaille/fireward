{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
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
, whileNot
, apply
, Parser
, guard
, failWith
, guardWith
, failure
, require
, ParserResult
, (<|>), (>>=)
, trim
) where 

import Data.Char ( isDigit , isLower , isUpper, isSpace)
import Data.List (dropWhileEnd)

import Control.Monad ((>>=), return, ap, liftM, guard)
import Control.Applicative (Alternative, (<|>), empty, many, some, optional)
import Prelude hiding (head)

type ParserError = Maybe (String, Int, Int)
type ParserSuccess a = (a, String, Int, Int) -- (result, unparsed, line, col, required)
type ParserResult a = Either ParserError (ParserSuccess a)

newtype Parser a = Parser (String -> Either ParserError (ParserSuccess a))

failure = Left . Just

maybeHead [] = Nothing
maybeHead (x:_) = Just x

apply :: Parser a -> String -> ParserResult a
apply (Parser p) s = p s

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

none :: Parser [a]
none = return []

failWith :: String -> Parser a
failWith msg = Parser (\s->failure (msg, 0, 0))

guardWith :: String -> Bool -> Parser ()
guardWith msg True = return ()
guardWith msg False = failWith msg


-- everyWith msg p = Parser q
--   where q s = res (apply (many p >>= (\r->space >> return r)) s)
--         res (Right (x, "", l, c, r)) = Right (x, "", l, c, r)
--         res (Right (x, u, l, c, r)) = Left (msg, l, c)
--         res error = error


instance Alternative Parser where
  empty = Parser (\s -> Left Nothing)
  p <|> q = Parser f where
    f s = let pick (Left Nothing) = apply q s
              pick ps@(Right x) = ps
              pick error = error
          in pick $ apply p s

instance Monad Parser where
  return x = Parser (\s -> Right (x, s, 0, 0))
  p >>= q = Parser outer
    where outer s = res (apply p s)
          res (Right (x, s', l', c')) = inner (apply (q x) s') l' c'
          res (Left error) = Left error
          inner (Right (y, s'', l'', c'')) l' c' = Right (y, s'', l'+l'', if l'' > 0 then c'' else c' + c'')
          inner (Left (Just (error, l'', c''))) l' c' = failure (error, l'+l'', if l'' > 0 then c'' else c' + c'')
          inner (Left Nothing) l' c' = Left Nothing
     
require msg p = Parser q
  where q s = res (apply p s)
        res val@(Right _) = val
        res (Left Nothing) = failure (msg, 0, 0)
        res e = e

ifLineSep c t f = if c=='\n' || c=='\r' then t else f

getc :: Parser Char
getc = Parser f where
  f [] = Left Nothing
  f (c:cs) = Right (c, cs, ifLineSep c 1 0 , ifLineSep c 0 1)

sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- getc; guard (p c); return c }

satS :: (Char -> Bool) -> Parser String
satS p = (:[]) <$> sat p 

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser ()
string "" = return ()
string (c:cs) = do { char c; string cs; return () }

whileNot :: Parser () -> Parser String
whileNot p = many_ next
  where next = do
          z <- optional p
          guard (z==Nothing)
          getc
        many_ p = do
          v <- p
          (v:) <$> (many_ p <|> return [])

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

