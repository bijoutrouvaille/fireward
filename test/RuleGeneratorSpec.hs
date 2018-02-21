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

repN (a:b:s) = if [a,b]=="\\n" 
                   then '\n':(repN s) else a:(repN (b:s))
repN s = s
showN = repN . show
g = RuleGenerator.generate
gt z = (\x->trace (showN x) x) (RuleGenerator.generate z)
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
      g "type X = Z | ZZ | {a:A, b?:B|BB, c:{ca:int, cb?:string}}"
      `shouldBe` ru
      [ "function isX (resource) {"
      , "  return isZ(resource)"
      , "  || isZZ(resource)"
      , "  || ("
      , "    resource.keys().hasAll(['a', 'c'])"
      , "    && resource.size() >= 2"
      , "    && resource.size() <= 3"
      , "    && isA(resource.a)"
      , "    && ("
      , "      !resource.hasAny(['b'])"
      , "      || ("
      , "         isB(resource.b)"
      , "      || isBB(resource.b)"
      , "      )"
      , "    )"
      , "    && ("
      , "      resource.c.keys().hasAll(['ca'])"
      , "      && resource.c.size() >= 1"
      , "      && resource.c.size() <= 2"
      , "      && resource.c.ca is int"
      , "      && ("
      , "        !resource.c.hasAny(['cb'])"
      , "        || resource.c.cb is string"
      , "      )"
      , "    )"
      , "  );"
      , "}" 
      ]
    it "generates a path with a type" $
      g "match x is X {}" `shouldBe` ru
        [ "match /x {"
        , "  function is__pathType (resource) {"
        , "    return isX(resource);"
        , "  }"
        , "  allow write: if is__pathType(request.resource.data);"
        , "}"
        ]

    it "generates a path with a type and custom write condition" $
      g "match x is X { allow create: if true; }" `shouldBe` ru
        [ "match /x {"
        , "  function is__pathType (resource) {"
        , "    return isX(resource);"
        , "  }"
        , "  allow create: if is__pathType(request.resource.data) && (true);"
        , "}"
        ]

