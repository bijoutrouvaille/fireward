module ParserSpec (main, spec) where

import Data.Char (isDigit)
import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Data.Char (isSpace)
import Parser

with x f = f x

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getc" $ do
    it "gets first char" $
      apply getc "hello" `shouldBe` [('h', "ello")]
  describe "sat" $ do
    it "gets first char if happy" $
      apply (sat isDigit) "1z" `shouldBe` [('1', "z")]
    it "gets a string until char" $
      apply (many $ sat (/=';')) "hello : ;" `shouldBe` [("hello : ", ";")]
  describe "char" $ do
    it "gets char" $ property $
      \c-> apply (char c) (c:"dioscuri") === [(c, "dioscuri")]
  describe "digit" $ do
    it "gets a digit" $
      apply digit "2b" `shouldBe` [('2', "b")]
  describe "<|>" $ do
    it "returns empty if not matched" $
      apply (digit <|> empty) "a2" `shouldBe` []
    it "returns a match if matched" $
      apply (digit <|> empty) "2a" `shouldBe` [('2', "a")]

  describe "some" $ do
    it "returns some digits" $
      apply (some digit) "123x" `shouldBe` [("123", "x")]
    it "alternates if some is not matched" $
      apply (some digit <|> some lower) "b123" `shouldBe` [("b", "123")]

  describe "many" $ do
    it "returns none if nothing is matched" $
      apply (many digit) "abc" `shouldBe` [([], "abc")]
  describe "optional'" $ do
    it "returns the item if matched" $
      apply (optional' . some $ digit) "1a" `shouldBe` [("1", "a")]
    it "returns the none if none matched" $
      apply (optional' . some $ digit) "a1" `shouldBe` [("", "a1")]

  describe "token" $ do
    it "parses a spaced number" $
      apply (token $ some digit) "  123x" `shouldBe` [("123", "x")]
  describe "symbol" $ do
    it "parses a spaced string" $
      apply (symbol "hello") "   hello " `shouldBe` [((), " ")]

  describe "grouped" $ do
    it "parses a number surrounded by brackets" $
      apply (grouped "[[" "]]" $ some digit) " [[123 ]] " `shouldBe` [("123", " ")]
    it "parses a group within group of same brackets" $
      apply (grouped "{" "}" $ do {
        symbol "+";
        grouped "{" "}" (some digit);
      }) "{ +{123}}" `shouldBe` [("123", "")]
    it "parses spaces and new lines" $
      apply (grouped "{" "}" (token digit)) (unlines
      [ " { "
      , " 3 "
      , " } " 
      ]) `shouldBe` [('3', " \n")]

  describe "oneof" $ do
    it "parses one of" $ forAll (elements "hzu") $
      \c -> apply (oneOf "hzu") (c:"hello") === [(c, "hello")]
    it "parses none of" $ forAll (elements "hzu") $
      \c -> apply (oneOf "abc") (c:"hello") === []

  describe "enum" $ do
    it "parses" $
      apply (enum ["aa", "bb"]) "aa" `shouldBe` [("aa", "")]
    it "parses manywith" $
      apply (manywith (symbol ",") $ enum [ "aa", "bb" ]) "aa,bb , bb, aa" 
        `shouldBe` [(["aa","bb","bb","aa"], "")]
  describe "somewith" $ do
    it "parses into an array" $
      apply (somewith (symbol ",") (token digit)) " 3, 4 , 5,6,3 ff+" `shouldBe` [("34563", " ff+")]
  
