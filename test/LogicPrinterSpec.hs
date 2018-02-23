module LogicPrinterSpec (main, spec) where

import LogicPrinter 
import Data.List (intercalate)
import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Debug.Trace (trace)

main :: IO ()
main = hspec spec

ru = intercalate "\n"
_or = Term Or
_and = Term And
_a = Atom




spec :: Spec
spec = do
  describe "LogicPrinter" $ do
    it "prints an expression" $
      show (Atom "x") `shouldBe` "x"
    it "groups operations correctly" $ do
      -- (a1 && (b1 || b2 || b3)) || a2
      let e = _or [ _and [ _a "a1" , _or [_a "b1", _or [_a "c1", _a "c2"], _and [_a "c3", _a "c4"],  _a "b2", _a "b3"] ] , _a "a2" ]
      
      show e `shouldBe` ru
        [ "( a1"
        , "  && ( b1"
        , "    || ( c1"
        , "      || c2"
        , "    )" 
        , "    || c3"
        , "    && c4"
        , "    || b2"
        , "    || b3"
        , "  )"
        , "  || a2"
        , ")"
        ]

      
      


