module ExprParserSpec (main, spec) where

import RuleParser

import Data.Char (isDigit)
import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Debug.Trace (trace)
import ExprParser
import ExprPrinter
import Parser (ParserSuccess(..))

p = printExpr

pos c l = show c ++ ":" ++ show l
etxt e c l = e ++ " " ++ pos c l
err (Just (e, c, l)) = etxt e c l
err Nothing = "Nothing"

ex e = print tree where
           tree = parse e
           print (Left e) = err e
           print (Right x) = printExpr x

parse s = d $ apply expr s where
  d (Left err) = Left err
  d (Right (ParserSuccess 
    { parserResult = x
    , unparsed = z
    , parserLine = _
    , parserCol = _
    , parserWarnings = w
    })) = if z=="" then Right x else Left (Just (z, 1, 1))
  -- d (Right (x, z, _, _)) = if z=="" then Right x else Left (Just (z, 1, 1))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Expression Parser" $ do
    it "passes for now" $
      shouldBe 1 1 
    it "parses an int" $ do
      parse "3;" `shouldBe` Right (ExprInt 3)
    it "parses a float" $ do
      parse "3.0;" `shouldBe` Right (ExprFloat 3.0)
    it "parses a disjunction" $
      ex "true || false" `shouldBe` "true || false"
    it "parses || and &&" $
      ex "1 || 2 && 3" `shouldBe` "1 || 2 && 3"
    it "parses equality" $ do
      ex "1==1" `shouldBe` "1 == 1"
    it "parses as operand" $ do
      ex "1==1 && 2==2" `shouldBe` "1 == 1 && 2 == 2"
    it "parses grouped expr" $
      ex "(1 + 2 == 4) || 5 || 6" `shouldBe` "(1 + 2 == 4) || 5 || 6" 
    it "parses an inner group" $
      ex " 1 +(2) + 3" `shouldBe`  "1 + (2) + 3"
    it "parses a strict eq" $
      ex "3===4" `shouldBe` "symbol === not allowed. 0:4"
    it "parses a dot access" $
      ex "hello.world == 3" `shouldBe` "hello.world == 3" 
    it "parses a function call with 1 param" $
      ex "f(g(1))" `shouldBe` "f(g(1))" 
    it "parses a function call with 2 params" $
      ex "f(g(1, 2))" `shouldBe` "f(g(1, 2))" 
    it "parses a function call with 1 expr param" $
      ex "f(g(1 || 2))" `shouldBe` "f(g(1 || 2))" 
    it "parses a function call with 1 var param" $
      ex "f(g(a))" `shouldBe` "f(g(a))" 
    it "parses a function call with 1 expr var param" $
      ex "g(a || b)" `shouldBe` "g(a || b)" 
    it "parses a nested function call with 1 expr var param" $
      ex "f(g(a || a.b.c))" `shouldBe` "f(g(a || a.b.c))" 
    it "parses a function call with 1 func param" $
      ex "f(g())" `shouldBe` "f(g())" 
    it "parses a unary op bang" $
      ex "!3 || 4" `shouldBe` "!3 || 4"
    it "parses a unary op +" $
      ex "+3 + +4" `shouldBe` "+3 + +4" 
    it "errs on an unexpected operator ?" $
      ex "3 + ?4" `shouldBe` "could not parse right side of operator + 0:3"
    it "fails on bad operators" $
      ex "4++" `shouldBe` "symbol ++ not allowed. 0:3"
    it "+++ fails" $
      ex "+++4" `shouldBe` "symbol ++ not allowed. 0:2" 
    it "parses a simple index" $
      ex "x[4]" `shouldBe` "x[4]"
    it "parses a complex index" $
      ex "a.b.c(hello)[a.b || !c.d + 5]" `shouldBe` "a.b.c(hello)[a.b || !c.d + 5]"
    it "parses an index inside index" $
      ex "x[f + x[4]]" `shouldBe` "x[f + x[4]]"
    it "errs on missing index bracket" $
      ex "f(x[3)" `shouldBe` "index missing closing bracket `]` 0:5"
    it "parses index range" $
      ex "x[4:7]" `shouldBe` "x[4:7]"
    it "parses expr index range" $
      ex "x[f(a.b) : g(p( n + 7))]" `shouldBe` "x[f(a.b):g(p(n + 7))]"

    -- Ternary Operator
    it "parses a basic ternary" $
      ex "a ? b : c" `shouldBe` "a ? b : c"
    it "parses a nested ternary" $
      ex "a ? b ? c : d : e" `shouldBe` "a ? b ? c : d : e"
    it "parses a nested ternary" $
      ex "a ? b : c ? d : e" `shouldBe` "a ? b : c ? d : e"
    it "parses a binary expression within a ternary" $
      ex "a || aa ? b || bb : c || cc" `shouldBe` "a || aa ? b || bb : c || cc"

    -- PATHS
    it "parses a simple path" $
      ex "(/docs)" `shouldBe` "(/docs)"
    it "parses a path with variable" $ 
      ex "(/docs/$(abc)/123)" `shouldBe` "(/docs/$(abc)/123)"
    it "parses a path with dot operator" $
      ex "(/hello/$(a.world)/1)" `shouldBe` "(/hello/$(a.world)/1)" 

    -- LISTS
    it "parses a number list" $
      ex "[ 1, 2  , 3 ]" `shouldBe`  "[1, 2, 3]"
    it "parses a string list" $
      ex "['a', 'b', 'c']" `shouldBe`  "['a', 'b', 'c']"
    it "parses an empty list" $
      ex "[]" `shouldBe`  "[]"
    it "parses an expr list" $
      ex "[a ||b, true && f() || 5]" `shouldBe`  "[a || b, true && f() || 5]"

    -- MISC
    it "parses a catenation with string" $
      ex "'a' + b" `shouldBe` "'a' + b"
    it "fails on missing list bracket" $
      ex "f(f.g([ 1, 2))" `shouldBe` "list missing closing bracket `]` 0:12"
    it "parses a map literal" $
      ex "hello.map({ \"a\": 3})" `shouldBe` "hello.map({ \"a\": 3 })" 
    it "parses a nested map literal" $
      ex "hello.map({ \"a\": { \"bac\": a.b.c()} })" `shouldBe` "hello.map({ \"a\": { \"bac\": a.b.c() } })" 
    it "parses a nested map literal" $
      ex "hello.map({ a: { bac: a.b.c()} })[123:123]" `shouldBe` "hello.map({ \"a\": { \"bac\": a.b.c() } })[123:123]" 
    it "parses a ' string" $
      ex "x('ff gg')" `shouldBe` "x('ff gg')"
    it "parses a \" string" $
      ex "x(\"ff gg\")" `shouldBe` "x(\"ff gg\")"
    it "parses a \" string with semicolon" $
      ex "x(\"ff;gg\")" `shouldBe` "x(\"ff;gg\")"
    it "parses a string equality" $ 
      ex "\"111\" == \"222\"" `shouldBe` "\"111\" == \"222\""
    it "parses a string equality" $ 
      ex "\"111\" == \"222\"" `shouldBe` "\"111\" == \"222\""
    it "parses an expression with inner semicolons" $ do
      ex "x==\"123;\" && y-3==4 ;" `shouldBe` "x == \"123;\" && y - 3 == 4"
    it "parses issue 11 func call" $ do
      ex "exists(/test/$(a)/$(b())/$(b(a)))" `shouldBe` "exists(/test/$(a)/$(b())/$(b(a)))"
    it "parses (issue 11) array within a func call" $ do
      ex "data().keys().hasAll(['a', 'b', 'c'])" `shouldBe` "data().keys().hasAll(['a', 'b', 'c'])"


        
    
