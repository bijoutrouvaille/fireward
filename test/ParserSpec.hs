module ParserSpec (main, spec) where

import Data.Char (isDigit)
import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Parser

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
  describe "char" $ do
    it "gets char" $ property $
      \c-> apply (char c) (c:"whateva") === [((), "whateva")]
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



