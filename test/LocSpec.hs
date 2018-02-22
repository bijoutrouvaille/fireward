module LocSpec (main, spec) where

import Loc
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
  describe "Location in code" $ do
    it "shows the only different line" $
      loc "some bad code" "bad code"
      `shouldBe`
      Just (Loc 0 "some bad code")
    it "shows the second line when the first is missing" $
      loc "line1\nline2" "line2" 
      `shouldBe` 
      Just (Loc 1 "line2")
    it "shows the second line when the first is missing and the second different" $
      loc "line1\nline2" "2" 
      `shouldBe` 
      Just (Loc 1 "line2")
    it "shows Nothing if the same" $
      loc "abc" "abc" `shouldBe` Nothing
    it "shows Nothing if remainder is longer than source" $
      loc "abc" "efg\nbcd" `shouldBe` Nothing
    it "shows Nothing if the source is empty" $
      loc "" "hello" `shouldBe` Nothing
