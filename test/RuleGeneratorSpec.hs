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

g = showE . RuleGenerator.generate False
gt z = (\x->trace (showN x) x) (g z)
gu = g . trim . unlines
r = ("Right " ++) . repA
ru = r . trim . unlines

startsWith :: String -> String -> Bool
startsWith str = (==str) . take (length str)

spec :: Spec
spec = do
  describe "Rule Generator" $ do
    it "generates a simple path" $
      g "match /x {}" `shouldBe` r "match /x {\n  \n}"
    it "generates a simple function" $
      g "function f(a,b,c) { return 123; }" 
      `shouldBe` 
      r "function f(a, b, c) {\n  return 123;\n}" 
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
        , "      "
        , "    }"
        , "  }"
        , "}"
        ]

    it "generates a type for all optional fields" $
      gu [ "type Z = {"
         , "  a?: string"
         , "}"
         ] `shouldBe` ru 
         [ "function is____Z(data, prev) {"
         , "  return data.size() >= 0 && data.size() <= 1"
         , "  && data.keys().hasOnly(['a'])"
         , "  && ("
         , "    !data.keys().hasAny(['a'])"
         , "    || data.a is string"
         , "  );"
         , "}"
         ]
    it "generates a type for all optional union fields" $
      gu [ "type Z = {"
         , "  a?: int|float"
         , "}"
         ] `shouldBe` ru 
         [ "function is____Z(data, prev) {"
         , "  return data.size() >= 0 && data.size() <= 1"
         , "  && data.keys().hasOnly(['a'])"
         , "  && ("
         , "    !data.keys().hasAny(['a'])"
         , "    || ("
         , "      data.a is int"
         , "      || (data.a is float || data.a is int)"
         , "    )"
         , "  );"
         , "}"
         ]
    it "generates a const type" $
      gu [ "type Z = {"
         , "  a: const string"
         , "}"
         ] `shouldBe` ru 
         [ "function is____Z(data, prev) {"
         , "  return data.keys().hasAll(['a'])"
         , "  && data.size() >= 1 && data.size() <= 1"
         , "  && data.keys().hasOnly(['a'])"
         , "  && (prev==null || !prev.keys().hasAll(['a']) || prev.a==null || data.a==prev.a)"
         , "  && data.a is string;"
         , "}"
         ]
    it "generates a optional const type" $
      gu [ "type Z = {"
         , "  a?: const string"
         , "}"
         ] `shouldBe` ru 
         [ "function is____Z(data, prev) {"
         , "  return data.size() >= 0 && data.size() <= 1"
         , "  && data.keys().hasOnly(['a'])"
         , "  && (prev==null || !prev.keys().hasAll(['a']) || prev.a==null || data.a==prev.a)"
         , "  && ("
         , "    !data.keys().hasAny(['a'])"
         , "    || data.a is string"
         , "  );"
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
         , "  function x(x) {"
         , "    return 123;"
         , "  }"
         , "}"
         ]
    it "generates a function from a type" $
      g "type X = Z | ZZ | {a:A, b?:B|BB, c:{ca:int, cb?:string}}"
      `shouldBe` ru
      [ "function is____X(data, prev) {"
      , "  return (prev==null && is____Z(data, null) || prev!=null && is____Z(data, prev))"
      , "  || (prev==null && is____ZZ(data, null) || prev!=null && is____ZZ(data, prev))"
      , "  || data.keys().hasAll(['a', 'c'])"
      , "  && data.size() >= 2 && data.size() <= 3"
      , "  && data.keys().hasOnly(['a', 'b', 'c'])"
      , "  && ((prev==null || !(prev.keys().hasAll['a'])) && is____A(data.a, null) || prev!=null && prev.keys().hasAll['a'] && is____A(data.a, prev.a))"
      , "  && ("
      , "    !data.keys().hasAny(['b'])"
      , "    || ("
      , "      ((prev==null || !(prev.keys().hasAll['b'])) && is____B(data.b, null) || prev!=null && prev.keys().hasAll['b'] && is____B(data.b, prev.b))"
      , "      || ((prev==null || !(prev.keys().hasAll['b'])) && is____BB(data.b, null) || prev!=null && prev.keys().hasAll['b'] && is____BB(data.b, prev.b))"
      , "    )"
      , "  )"
      , "  && data.c.keys().hasAll(['ca'])"
      , "  && data.c.size() >= 1 && data.c.size() <= 2"
      , "  && data.c.keys().hasOnly(['ca', 'cb'])"
      , "  && data.c.ca is int"
      , "  && ("
      , "    !data.c.keys().hasAny(['cb'])"
      , "    || data.c.cb is string"
      , "  );"
      , "}" 
      ]
    it "generates a check of definite size" $
      g "type X = { x: string[2] }"
      `shouldBe` ru
        [ "function is____X(data, prev) {"
        , "  return data.keys().hasAll(['x'])"
        , "  && data.size() >= 1 && data.size() <= 1"
        , "  && data.keys().hasOnly(['x'])"
        , "  && (data.x is list && data.x.size() <= 2 && data.x.size() >= 0"
        , "    && ("
        , "      !(data.x is list && data.x.size() > 0) ||  data.x[0] == null  || data.x[0] is string"
        , "    )"
        , "    && ("
        , "      !(data.x is list && data.x.size() > 1) ||  data.x[1] == null  || data.x[1] is string"
        , "    ));"
        , "}"
        ]


    
      -- [ "function is____X(data, prev) {"
      -- , "  return data.keys().hasAll(['x'])"
      -- , "  && data.size() >= 1 && data.size() <= 1"
      -- , "  && data.keys().hasOnly(['x'])"
      -- , "  && data.x is list"
      -- , "  && (data.x.size() <= 1 || data.x[0] is string)"
      -- , "  && (data.x.size() <= 2 || data.x[1] is string);"
      -- , "}"
      -- ]
    
    it "creates a correct check for empty type object" $
      g "type X = {}" `shouldBe` ru
        [ "function is____X(data, prev) {"
        , "  return data.size() >= 0 && data.size() <= 0"
        , "  && data.keys().hasOnly([])"
        , "  ;"
        , "}"
        ]
        

    it "generates a path with a type" $
      g "match x is X {}" `shouldBe` ru
        [ "match /x {"
        , "  function is______PathType(data, prev) {"
        , "    return (prev==null && is____X(data, null) || prev!=null && is____X(data, prev));"
        , "  }"
        , "  allow write: if (resource==null && is______PathType(request.resource.data, null) || is______PathType(request.resource.data, resource.data));"
        , "}"
        ]

    it "generates a path with a type and custom write condition" $
      g "match x is X { allow create: if true; }" `shouldBe` ru
        [ "match /x {"
        , "  function is______PathType(data, prev) {"
        , "    return (prev==null && is____X(data, null) || prev!=null && is____X(data, prev));"
        , "  }"
        , "  allow create: if (resource==null && is______PathType(request.resource.data, null) || is______PathType(request.resource.data, resource.data)) && (true);"
        , "}\n"
        ]

    it "generates validations for types" $
      gu
      [ "type X = {"
      , "a: string"
      , "allow create, delete: if prev.a == data.a || 1==1"
      , "allow update, delete: 2==2 && 3==3"
      , "allow write: true"
      , "}"
      ] `shouldBe` ru
        [ "function is____X(data, prev) {"
        , "  return ("
        , "    ( request.method != 'create' || ( prev.a == data.a || 1 == 1 ) )"
        , "    && ( request.method != 'delete' || ( prev.a == data.a || 1 == 1 ) )"
        , "    && ( request.method != 'update' || ( 2 == 2 && 3 == 3 ) )"
        , "    && ( request.method != 'delete' || ( 2 == 2 && 3 == 3 ) )"
        , "    && ( request.method != 'create' || ( true ) )"
        , "    && ( request.method != 'update' || ( true ) )"
        , "    && ( request.method != 'delete' || ( true ) )"
        , "  ) && data.keys().hasAll(['a'])"
        , "  && data.size() >= 1 && data.size() <= 1"
        , "  && data.keys().hasOnly(['a'])"
        , "  && data.a is string;"
        , "}"
        ]

    it "errors on empty list of request methods in validation" $ 
      gu
      [ "type X = {"
      , "a: string,"
      , "allow create: true"
      , "allow: true"
      , "}"
      ] `shouldBe` "Left Validation expression must contain at least one request method (create, update, delete)\n  on line 4, column 6"

      

    it "indents a complex file" $ do
      ward <- readFile "test/fixtures/indent.ward"
      _rule <- readFile "test/fixtures/indent.rules"
      let rule = take ((length _rule) - 1) _rule
      res <- return (g ward)
      g ward `shouldBe` r rule
  describe "Literal Types" $ do
    it "handle a boolean" $
      g "type X = { a: true }" `shouldBe` "Right function is____X(data, prev) {\n  return data.keys().hasAll(['a'])\n  && data.size() >= 1 && data.size() <= 1\n  && data.keys().hasOnly(['a'])\n  && data.a == true;\n}" 
    it "handle a string" $ do
      g "type X = { a: 'me' | \"you\" }" `shouldBe` "Right function is____X(data, prev) {\n  return data.keys().hasAll(['a'])\n  && data.size() >= 1 && data.size() <= 1\n  && data.keys().hasOnly(['a'])\n  && (\n    data.a == 'me'\n    || data.a == \"you\"\n  );\n}"
    it "handle a float" $
      g "type X = { a: 123.3 }" `shouldBe` "Right function is____X(data, prev) {\n  return data.keys().hasAll(['a'])\n  && data.size() >= 1 && data.size() <= 1\n  && data.keys().hasOnly(['a'])\n  && data.a == 123.3;\n}" 
  describe "Smoke Test" $ do
    it "passes" $ do
      ward <- readFile "examples/smoke-test.ward"
      g ward `shouldSatisfy` (not . startsWith "Left")
