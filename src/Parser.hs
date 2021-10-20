{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

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
, charIn
, whileNot
, apply
, Parser
, unparsable
, guard
, failWith
, guardWith
, failure
, require
, ParserResult
, ParserSuccess(..)
, (<|>), (>>=)
, trim
) where 

-- import Data.Map.Strict 
import Data.Char ( isDigit , isLower , isUpper, isSpace)
import Data.List (dropWhileEnd, lookup)

import Control.Monad ((>>=), return, ap, liftM, guard)
import Control.Applicative (Alternative, (<|>), empty, many, some, optional)
import Prelude hiding (head)

data Warning = Warning { warningText::String, warningLine::Int, warningCol::Int }
type ParserError = Maybe (String, Int, Int)
data ParserSuccess a = ParserSuccess 
  { parserResult::a
  , unparsed::String
  , parserLine::Int 
  , parserCol::Int 
  , parserWarnings::[Warning]
  } -- (result, unparsed, line, col, required)
type ParserResult a = Either ParserError (ParserSuccess a)

newtype Parser a = Parser (String -> Either ParserError (ParserSuccess a))


maybeHead [] = Nothing
maybeHead (x:_) = Just x

apply :: Parser a -> String -> ParserResult a
apply (Parser p) s = p s

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

-- this low-level function represents a critical failure that
-- should produce error output to the user.
failure = Left . Just

-- This represents a point at which the parser does not know what to do.
-- Unlike a failure, this will not produce a parser error. This combinator
-- explicitly says: this is not the structure you're looking for. 
-- `guard True` does a similar thing, but guard returns () but `unparsable` 
-- returns the generic Parser a, so you can do things like 
-- `if cond then return myValue else unparsable`
unparsable :: Parser a
unparsable = Parser $ \s -> Left Nothing

-- this ends the compilation with an error message intended for the user
-- `if null closingParen then failWith "Missing closing paren" else return myResult`
failWith :: String -> Parser a
failWith msg = Parser (\s->failure (msg, 0, 0))

guardWith :: String -> Bool -> Parser ()
guardWith msg True = return ()
guardWith msg False = failWith msg

-- This represents an empty result set for parsers that look for multiple results.
none :: Parser [a]
none = return []

instance Alternative Parser where
  empty = Parser (\s -> Left Nothing)
  p <|> q = Parser f where
    f s = let pick (Left Nothing) = apply q s
              pick ps@(Right x) = ps
              pick error = error
          in pick $ apply p s

instance Monad Parser where
  return x = Parser (\s -> Right $ ParserSuccess x s 0 0 [])
  p >>= q = Parser outer
    where outer s = res (apply p s)
          res (Right (ParserSuccess { 
            parserResult = x, 
            unparsed = s', 
            parserLine = l', 
            parserCol = c', 
            parserWarnings = w'
          })) = inner (apply (q x) s') l' c' w'
          res (Left error) = Left error
          inner (Right (ParserSuccess
            { parserResult = y
            , unparsed = s''
            , parserLine = l''
            , parserCol = c''
            , parserWarnings = w''
            })) l' c' w' = Right $ ParserSuccess
              { parserResult = y
              , unparsed = s''
              , parserLine = l' + l''
              , parserCol = if l'' > 0 then c'' else c' + c''
              , parserWarnings = w' ++ w''
              }
            -- (y, s'', l'+l'', if l'' > 0 then c'' else c' + c'')
          -- (y, s'', l'', c'')) l' c' = Right (y, s'', l'+l'', if l'' > 0 then c'' else c' + c'')
          inner (Left (Just (error, l'', c''))) l' c' w' = failure (error, l'+l'', if l'' > 0 then c'' else c' + c'')
          inner (Left Nothing) l' c' w' = Left Nothing
     
require msg p = Parser q
  where q s = res (apply p s)
        res val@(Right _) = val
        res (Left Nothing) = failure (msg, 0, 0)
        res e = e

ifLineSep c t f = if c=='\n' || c=='\r' then t else f

getc :: Parser Char
getc = Parser f where
  f [] = Left Nothing
  -- f (c:cs) = Right (c, cs, ifLineSep c 1 0 , ifLineSep c 0 1)
  f (c:cs) = Right $ ParserSuccess
    { parserResult = c
    , unparsed = cs
    , parserLine = ifLineSep c 1 0
    , parserCol = ifLineSep c 0 1
    , parserWarnings = []
    }

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

charIn :: [Char] -> Parser Char
charIn "" = empty
charIn (c:cs) = char c <|> charIn cs

_const :: String -> Parser String
_const s = do
  string s
  return s
  

alt :: (a -> Parser a) -> [a] -> Parser a
alt p [] = unparsable
alt p (x:xs) = p x <|> alt p xs

enum :: [String] -> Parser String
enum xs = alt (\s -> do { symbol s ; return s }) xs

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
