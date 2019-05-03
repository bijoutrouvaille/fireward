module ParserSpec (main, spec) where

import Data.Char (isDigit)
import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Data.Char (isSpace)
import Parser

with x f = f x

_apply p s = res (apply p s) 
  where res (Right (x, u, l, c)) = Right (x, u)
        res (Left x) = Left x


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getc" $ do
    it "gets first char" $
      _apply getc "hello" `shouldBe` Right ('h', "ello")
  describe "sat" $ do
    it "gets first char if happy" $
      _apply (sat isDigit) "1z" `shouldBe` Right ('1', "z")
    it "gets a string until char" $
      _apply (many $ sat (/=';')) "hello : ;" `shouldBe` Right ("hello : ", ";")
  -- describe "space" $ do
  --   it "eats line comments" $ do
  --     let s = many $ sat (/=';')
  --     _apply (many s) "hello // this is a comment\nbye" `shouldBe` [(["hello", "bye"], "")]
  describe "char" $ do
    it "gets char" $ property $
      \c-> _apply (char c) (c:"dioscuri") === Right (c, "dioscuri")
  describe "digit" $ do
    it "gets a digit" $
      _apply digit "2b" `shouldBe` Right ('2', "b")
  describe "<|>" $ do
    it "returns empty if not matched" $
      _apply (digit <|> empty) "a2" `shouldBe` Left Nothing
    it "returns a match if matched" $
      _apply (digit <|> empty) "2a" `shouldBe` Right ('2', "a")

  describe "some" $ do
    it "returns some digits" $
      _apply (some digit) "123x" `shouldBe` Right ("123", "x")
    it "alternates if some is not matched" $
      _apply (some digit <|> some lower) "b123" `shouldBe` Right ("b", "123")

  describe "many" $ do
    it "returns none if nothing is matched" $
      _apply (many digit) "abc" `shouldBe` Right ([], "abc")
  describe "optional'" $ do
    it "returns the item if matched" $
      _apply (optional' . some $ digit) "1a" `shouldBe` Right ("1", "a")
    it "returns the none if none matched" $
      _apply (optional' . some $ digit) "a1" `shouldBe` Right ("", "a1")
  
  describe "token" $ do
    it "parses a spaced number" $
      _apply (token $ some digit) "  123x" `shouldBe` Right ("123", "x")
  describe "symbol" $ do
    it "parses a spaced string" $
      _apply (symbol "hello") "   hello " `shouldBe` Right ((), " ")

  describe "grouped" $ do
    it "parses a number surrounded by brackets" $
      _apply (grouped "[[" "]]" $ some digit) " [[123 ]] " `shouldBe` Right ("123", " ")
    it "parses a group within group of same brackets" $
      _apply (grouped "{" "}" $ do {
        symbol "+";
        grouped "{" "}" (some digit);
      }) "{ +{123}}" `shouldBe` Right ("123", "")
    it "parses spaces and new lines" $
      _apply (grouped "{" "}" (token digit)) (unlines
      [ " { "
      , " 3 "
      , " } " 
      ]) `shouldBe` Right ('3', " \n")

  describe "whileNot" $ do
    it "parses until a keyword" $ do
      _apply (whileNot $ symbol "end") "123 hello, end" `shouldBe` Right ("123 hello,", " end")
    it "parses alternatives until" $
      _apply (whileNot $ symbol "end" <|> (char ';' >> return ())) "he llo;, end" `shouldBe` Right ("he llo", ";, end")
  describe "oneof" $ do
    it "parses one of" $ forAll (elements "hzu") $
      \c -> _apply (oneOf "hzu") (c:"hello") === Right (c, "hello")
    it "parses none of" $ forAll (elements "hzu") $
      \c -> _apply (oneOf "abc") (c:"hello") === Left Nothing

  describe "enum" $ do
    it "parses" $
      _apply (enum ["aa", "bb"]) "aa" `shouldBe` Right ("aa", "")
    it "parses manywith" $
      _apply (manywith (symbol ",") $ enum [ "aa", "bb" ]) "aa,bb , bb, aa" 
        `shouldBe` Right (["aa","bb","bb","aa"], "")
  describe "somewith" $ do
    it "parses into an array" $
      _apply (somewith (symbol ",") (token digit)) " 3, 4 , 5,6,3 ff+" `shouldBe` Right ("34563", " ff+")
  describe "require" $ do
    it "fails on missing requirements" $ do
      let p = do symbol "start"
                 num <- token  $ some digit
                 require "expecting the end" $ symbol "end"
                 return num

      _apply p "start\n123" `shouldBe` failure ("expecting the end", 1, 3)

    it "fails on a bad alternative" $ do
      let q = symbol "{" >> require "digit required" (some digit)
      let p = string "{" >> return "3"
      _apply (q <|> p) "{" `shouldBe` failure ("digit required", 0, 1)


  
