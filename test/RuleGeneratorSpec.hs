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
showE (Left x) = "Left " ++ repA x
-- showE (Left Nothing) = "Left Nothing"
g = showE . RuleGenerator.generate False
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
    -- it "generates a type for all optional fields from parsed" $
    --   
    --   g (Right ([TopLevelType "X" [
    --     InlineTypeRef (TypeDef [Field {required = False, fieldName = "x", typeRefs = [TypeNameRef "string" Nothing], constant = False}])]]
    --   ,""))
    --   `shouldBe` ru
    --    [ "function isZ(data, prev) {"
    --    , "  return data.keys().size() >= 0"
    --    , "    && data.size() <= 1"
    --    , "    && ("
    --    , "      !data.keys().hasAny(['a'])"
    --    , "      || data.a is string"
    --    , "    );"
    --    , "}"
    --    ]

    it "generates a type for all optional fields" $
      gu [ "type Z = {"
         , "  a?: string"
         , "}"
         ] `shouldBe` ru 
         [ "function isZ(data, prev) {"
         , "  return data.size() >= 0"
         , "    && data.size() <= 1"
         , "    && ("
         , "      !data.keys().hasAny(['a'])"
         , "      || data.a is string"
         , "    );"
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
      [ "function isX(data, prev) {"
      , "  return (prev==null && isZ(data, null) || isZ(data, prev))"
      , "  || (prev==null && isZZ(data, null) || isZZ(data, prev))"
      , "  || data.keys().hasAll(['a', 'c'])"
      , "    && data.size() >= 2"
      , "    && data.size() <= 3"
      , "    && (prev==null && isA(data.a, null) || isA(data.a, prev))"
      , "    && ("
      , "      !data.keys().hasAny(['b'])"
      , "      || ("
      , "         (prev==null && isB(data.b, null) || isB(data.b, prev))"
      , "      || (prev==null && isBB(data.b, null) || isBB(data.b, prev))"
      , "      )"
      , "    )"
      , "    && data.c.keys().hasAll(['ca'])"
      , "      && data.c.size() >= 1"
      , "      && data.c.size() <= 2"
      , "      && data.c.ca is int"
      , "      && ("
      , "        !data.c.keys().hasAny(['cb'])"
      , "        || data.c.cb is string"
      , "      );"
      , "}" 
      ]
    it "generates a check of definite size" $
      g "type X = { x: string[2] }"
      `shouldBe` ru
      [ "function isX(data, prev) {"
      , "  return data.keys().hasAll(['x'])"
      , "    && data.size() >= 1"
      , "    && data.size() <= 1"
      , "    && data.x is list"
      , "    && (data.x.size() <= 1 || data.x[0] is string)"
      , "    && (data.x.size() <= 2 || data.x[1] is string);"
      , "}"
      ]
      
    it "generates a path with a type" $
      g "match x is X {}" `shouldBe` ru
        [ "match /x {"
        , "  function is__PathType(data, prev) {"
        , "    return (prev==null && isX(data, null) || isX(data, prev));"
        , "  }"
        , "  allow write: if (resource==null && is__PathType(request.resource.data, null) || is__PathType(request.resource.data, resource.data));"
        , "}"
        ]

    it "generates a path with a type and custom write condition" $
      g "match x is X { allow create: if true; }" `shouldBe` ru
        [ "match /x {"
        , "  function is__PathType(data, prev) {"
        , "    return (prev==null && isX(data, null) || isX(data, prev));"
        , "  }"
        , "  allow create: if (resource==null && is__PathType(request.resource.data, null) || is__PathType(request.resource.data, resource.data)) && (true);"
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
