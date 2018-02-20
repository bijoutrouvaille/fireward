module RuleGeneratorSpec (main, spec) where

import Parser (trim)
import RuleParser
import RuleGenerator

import Data.Char (isDigit)
import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Debug.Trace (trace)

main :: IO ()
main = hspec spec

g = RuleGenerator.generate
gu = RuleGenerator.generate . trim . unlines
ru = Right . trim . unlines

spec :: Spec
spec = do
  describe "Rule Generator" $ do
    it "generates a simple path" $
      g "match /x {}" `shouldBe` Right "match /x {\n}"
    it "generates a simple function" $
      g "function f(a,b,c) { return 123; }" 
      `shouldBe` 
      Right "function f(a, b, c) { return 123; }" 
    it "generates a path in path" $
      gu 
        [ "match /a/{b} {"
        , "  match /x/y {"
        , "    match /x/y {"
        , "    }"
        , "  }"
        , "}"
        ] `shouldBe` ru
        [ "match /a/{b} {"
        , "  match /x/y {"
        , "    match /x/y {"
        , "    }"
        , "  }"
        , "}"
        ]
    
    it "generates a function in a path" $
      gu [ "match a {"
         , "  function x(x) {"
         , "    return 123;"
         , "  }"
         , "}"
         ] `shouldBe` ru 
         [ "match /a {"
         , "  function x(x) { return 123; }"
         , "}"
         ]
    it "generates a function from a type" $
      g "type X = A | {b:B|BB}"
      `shouldBe` ru
      [ "function isX (resource) {"
      , "  return isA(resource)"
      , "  || resource.keys().hasAll(['b'])"
      , "  && resource.size() >= 1"
      , "  && resource.size() <= 1"
      , "  && (isB(resource.b)"
      , "  || isBB(resource.b));"
      , "}" 
      ]
    it "generates a path with a type" $
      g "match x is X {}" `shouldBe` ru
        [ "match /x {"
        , "  function is__pathType (resource) {"
        , "    return isX(resource);"
        , "  }"
        , "  allow write: if is__pathType(resource);"
        , "}"
        ]


