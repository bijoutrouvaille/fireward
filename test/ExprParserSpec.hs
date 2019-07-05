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

p = printExpr

ex e = print tree where
           tree = parse e
           print (Left err) = show err
           print (Right x) = printExpr x

parse s = d $ apply expr s where
  d (Left err) = Left err
  d (Right (x, z, _, _)) = if z=="" then Right x else Left (Just (z, 1, 1))

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
    it "parses grouped expr" $
      ex "(1 + 2 == 4) || 5 || 6" `shouldBe` "(1 + 2 == 4) || 5 || 6" 
    it "parses an inner group" $
      ex " 1 +(2) + 3" `shouldBe`  "1 + (2) + 3"
    it "parses a strict eq" $
      ex "3===4" `shouldBe` "3 === 4"
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

        
    
