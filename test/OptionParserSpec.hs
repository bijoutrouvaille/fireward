module OptionParserSpec (main, spec) where

import Data.Char (isDigit)
import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Debug.Trace (trace)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Option Parser" $ do
    it "passes" $
      shouldBe 1 1

