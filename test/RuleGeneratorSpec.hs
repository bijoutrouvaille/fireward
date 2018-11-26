module RuleGeneratorSpec (main, spec) where

import Parser (trim)
import Error
import Loc
import RuleParser
import RuleGenerator

import Data.Char (isDigit)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Test.Hspec
import Test.QuickCheck
import Debug.Trace (trace)

main :: IO ()
main = hspec spec

esc c r (a:b:s) = if [a,b]==['\\', c] 
                   then r:(esc c r s) else a:(esc c r (b:s))
esc c r s = s
repN = esc 'n' '\n'
repQ = esc '"' '\"'
repA = repN . repQ
showN :: Show a => a -> String
showN = repA . show
showE (Right x) = "Right " ++ repA x
showE (Left (Error loc x)) = "Left " ++ repA x
g = showE . RuleGenerator.generate
gt z = (\x->trace (showN x) x) (g z)
gu = g . trim . unlines
r = ("Right " ++) . repA
ru = r . trim . unlines

spec :: Spec
spec = do
  describe "Rule Generator" $ do
    it "generates a simple path" $
      g "match /x {}" `shouldBe` r "match /x {\n}"
    it "generates a simple function" $
      g "function f(a,b,c) { return 123; }" 
      `shouldBe` 
      r "function f(a, b, c) { return 123; }" 
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
      , "  || resource.keys().hasAll(['a', 'c'])"
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
      , "    && resource.c.keys().hasAll(['ca'])"
      , "      && resource.c.size() >= 1"
      , "      && resource.c.size() <= 2"
      , "      && resource.c.ca is int"
      , "      && ("
      , "        !resource.c.hasAny(['cb'])"
      , "        || resource.c.cb is string"
      , "      );"
      , "}" 
      ]
    it "generates a check of definite size" $
      g "type X = { x: string[2] }"
      `shouldBe` ru
      [ "function isX (resource) {"
      , "  return resource.keys().hasAll(['x'])"
      , "    && resource.size() >= 1"
      , "    && resource.size() <= 1"
      , "    && resource.x is list"
      , "    && (resource.x.size() <= 1 || resource.x[0] is string)"
      , "    && (resource.x.size() <= 2 || resource.x[1] is string);"
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
    -- it "generates a definite array" $
    --   g "match 
    it "indents a complex file" $ do
      door <- readFile "test/fixtures/indent.door"
      _rule <- readFile "test/fixtures/indent.rules"
      let rule = take ((length _rule) - 1) _rule
      res <- return (g door)
      g door `shouldBe` r rule
      -- (trace (showN res)) (return 1)
      -- shouldBe 1 1
