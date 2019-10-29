module CodePrinter (
  printCode,
  _print,
  _return,
  _blank,
  _indent,
  _deindent,
  _indented,
  _getIndent,
  _linesWith,
  _line,
  _lines,
  _printIf,
  _printMany,
  CodePrinter
  ) where

import Data.Char (isDigit , isLower , isUpper, isSpace)
import Data.List (dropWhileEnd)

import Control.Monad ((>>=), return, ap, liftM, guard)
import Control.Applicative (Alternative, (<|>), empty, many, some, optional)
import Prelude hiding (head)



newtype State s a = State (s -> (a, s))

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

apply (State p) s = p s
state :: (s -> (a, s)) -> State s a
state f = State (\s -> f s)

instance Monad (State s) where
  return x = state (\s -> (x, s))
  p >>= k = state $ \ s0 ->
    let (x, s1) = apply p s0
     in apply (k x) s1

put newState = state $ \_ -> ((), newState)
get = state $ \s -> (s, s)

type CodePrinter = State (String, Int) ()

cr = "\n"
space :: Int -> String
space n = take n $ repeat ' '
_getIndent = do
  (code, i)<-get
  return i

_print s = do
  (c,i) <- get
  put (c ++ s, i)

_return = do
  i <- _getIndent
  _print cr
  _print $ space (i * 2)

_line s = do
  _return
  s

_shift :: Int -> CodePrinter
_shift n = do
  (code, i) <- get
  put (code, if i + n >= 0 then i + n else 0)

_indent = do
  _shift 1
_deindent = do
  _shift (-1)

_indented p = do
  _indent
  p
  _deindent

_blank :: CodePrinter
_blank = state (\s->((), s))
_manyWith s [] = _print ""
_manyWith s (p:[]) = p
_manyWith s (p:ps) = p >> s >> _manyWith s ps

_printMany [] = _blank
_printMany (p:printers) = p >> _printMany printers


_lines ps = _manyWith _return ps
_linesWith s printers = _manyWith (_return >> s) printers
  
_printIf condition true = do
  if condition then true else _print ""


printCode initialIndent printer = fst . snd $ apply printer ("", initialIndent)
