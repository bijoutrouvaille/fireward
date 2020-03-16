module RuleLang (
  _function,
  _hasAny,
  _hasAll,
  _hasOnly,
  _allow,
  _sizeLte,
  _sizeGte,
  _sizeBetween,
  _pathBlock,
  _and, _or
  ) where

import Parser
import RuleParser
import Error (Error(..))
import Loc (loc, Loc)
import Data.List (findIndices, intercalate, stripPrefix)
import Data.Char (toUpper)
import Data.Maybe (maybe)
import CodePrinter

spaces n = take n $ repeat ' '
moveBy n = (spaces (n*2) ++)

_quote x = "'" ++ x ++ "'"
_quoteList x = _list $ _quote <$> x
_list = intercalate ", "

_hasAny parent elements = parent ++ ".keys().hasAny([" ++ _quoteList elements ++ "])"
_hasAll parent elements = parent ++ ".keys().hasAll([" ++ _quoteList elements ++ "])"
_hasOnly parent elements = parent ++ ".keys().hasOnly([" ++ _quoteList elements ++ "])"

_allow conditions expr = do
  _print $ "allow " ++ _list conditions ++ ": "
  expr

_sizeLte item max = item ++ ".size() <= " ++ show max
_sizeGte item min = item ++ ".size() >= " ++ show min
_sizeBetween item min max = _sizeGte item min ++ " && " ++ _sizeLte item max

_and = _print "&& "
_or = _print "|| "

_function name params vars body = do
  _print "function "
  _print name
  _print "("
  _print $ _list params
  _print ") {"
  _indent
  _return
  _lines [ _print $ "let " ++ vname ++ " = " ++ vval ++ ";" | (vname, vval) <- vars ]
  _printIf (length vars > 0) _return
  _print "return "
  body
  _print ";"
  _deindent
  _return
  _print "}"

_pathBlock parts typeFunc bodyItems= do
  _print "match /"
  _printMany parts
  _print " {"
  _indent
  _return
  typeFunc
  _return
  bodyItems
  _deindent
  _return
  _print "}"
  _return
  
