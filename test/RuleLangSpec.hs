module RuleLangSpec (main, spec) where

import RuleLang
import CodePrinter
import Error

import Data.Char (isDigit)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Test.Hspec
import Test.QuickCheck
import Debug.Trace (trace)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rule Language Printer" $ do
    it "prints a line" $ do
      printCode 0 (_blank >> _indent >> _return) `shouldBe` "\n  "
