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
gv = showE . RuleGenerator.generate True
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
         , "  return data.keys().hasOnly(['a'])"
         , "  && ("
         , "    !data.keys().hasAny(['a'])"
         , "    || data['a'] is string"
         , "  );"
         , "}"
         ]
    it "generates a type for all optional union fields" $
      gu [ "type Z = {"
         , "  a?: int|float"
         , "}"
         ] `shouldBe` ru 
         [ "function is____Z(data, prev) {"
         , "  return data.keys().hasOnly(['a'])"
         , "  && ("
         , "    !data.keys().hasAny(['a'])"
         , "    || ("
         , "      data['a'] is int"
         , "      || (data['a'] is float || data['a'] is int)"
         , "    )"
         , "  );"
         , "}"
         ]
    it "generates a readonly type" $
      gu [ "type Z = {"
         , "  readonly a: string"
         , "}"
         ] `shouldBe` ru 
         [ "function is____Z(data, prev) {"
         , "  return data.keys().hasAll(['a'])"
         , "  && data.keys().hasOnly(['a'])"
         , "  && (!(prev!=null && 'a' in prev) || data['a']==prev['a'] || prev['a'] is map && data['a'] is map && data['a'].diff(prev['a']).changedKeys().size() == 0)"
         , "  && data['a'] is string;"
         , "}"
         ]
    it "generates a optional const type" $ do
      gu [ "type Z = {"
         , "  a?: const string"
         , "}"
         ] `shouldBe` ru 
         [ "function is____Z(data, prev) {"
         , "  return data.keys().hasOnly(['a'])"
         , "  && (!(prev!=null && 'a' in prev) || data['a']==prev['a'] || prev['a'] is map && data['a'] is map && data['a'].diff(prev['a']).changedKeys().size() == 0)"
         , "  && ("
         , "    !data.keys().hasAny(['a'])"
         , "    || data['a'] is string"
         , "  );"
         , "}"                                       
         ]
    describe "path functions" $
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
           ];
      it "generates a function with variables" $
        gu [ "match a {"
           , "  function x(x) {"
           , "    let z= 123+ false;"
           , "    return z;"
           , "  }"
           , "}"
           ] `shouldBe` ru 
           [ "match /a {"
           , "  function x(x) {"
           , "    let z = 123 + false;"
           , "    return z;"
           , "  }"
           , "}"
           ]

    it "generates a function from a type" $
      g "type X = Z | ZZ | {a:A, b?:B|BB, c:{ca:int, cb?:string}}"
      `shouldBe` ru
      [ "function is____X(data, prev) {"
      , "  return is____Z(data, prev!=null ? prev : null)"
      , "  || is____ZZ(data, prev!=null ? prev : null)"
      , "  || data.keys().hasAll(['a', 'c'])"
      , "  && data.keys().hasOnly(['a', 'b', 'c'])"
      , "  && is____A(data['a'], prev!=null && 'a' in prev ? prev['a'] : null)"
      , "  && ("
      , "    !data.keys().hasAny(['b'])"
      , "    || ("
      , "      is____B(data['b'], prev!=null && 'b' in prev ? prev['b'] : null)"
      , "      || is____BB(data['b'], prev!=null && 'b' in prev ? prev['b'] : null)"
      , "    )"
      , "  )"
      , "  && data['c'].keys().hasAll(['ca'])"
      , "  && data['c'].keys().hasOnly(['ca', 'cb'])"
      , "  && data['c']['ca'] is int"
      , "  && ("
      , "    !data['c'].keys().hasAny(['cb'])"
      , "    || data['c']['cb'] is string"
      , "  );"
      , "}" 
      ]

    it "generates a function with a readonly nested type" $
      g "type X = {a:A, readonly c:{ca:int}}"
      `shouldBe` ru
      [ "function is____X(data, prev) {"
      , "  return data.keys().hasAll(['a', 'c'])"
      , "  && data.keys().hasOnly(['a', 'c'])"
      , "  && is____A(data['a'], prev!=null && 'a' in prev ? prev['a'] : null)"
      , "  && (!(prev!=null && 'c' in prev) || data['c']==prev['c'] || prev['c'] is map && data['c'] is map && data['c'].diff(prev['c']).changedKeys().size() == 0)"
      , "  && data['c'].keys().hasAll(['ca'])"
      , "  && data['c'].keys().hasOnly(['ca'])"
      , "  && data['c']['ca'] is int;"
      , "}" 
      ]

    it "generates a function with a readonly nested type and allow conditions" $
      g "type X = {a:A, readonly c:{ca:int, allow create: true} allow write: true}"
      `shouldBe` ru
      [ "function is____X(data, prev) {"
      , "  return ("
      , "    ( request.method != 'create' || ( true ) )\n    && ( request.method != 'update' || ( true ) )\n    && ( request.method != 'delete' || ( true ) )"
      , "  ) && data.keys().hasAll(['a', 'c'])"
      , "  && data.keys().hasOnly(['a', 'c'])"
      , "  && is____A(data['a'], prev!=null && 'a' in prev ? prev['a'] : null)"
      , "  && (!(prev!=null && 'c' in prev) || data['c']==prev['c'] || prev['c'] is map && data['c'] is map && data['c'].diff(prev['c']).changedKeys().size() == 0)"
      , "  && ("
      , "    ( request.method != 'create' || ( true ) )"
      , "  ) && data['c'].keys().hasAll(['ca'])"
      , "  && data['c'].keys().hasOnly(['ca'])"
      , "  && data['c']['ca'] is int;"
      , "}" 
      ]
    it "generates a custom type tuple" $ 
      g "type X = {y: [Y, Y?]}" `shouldBe` ru
        [ "function is____X(data, prev) {"
        , "  return data.keys().hasAll(['y'])"
        , "  && data.keys().hasOnly(['y'])"
        , "  && ( data['y'] is list && data['y'].size() <= 2 && data['y'].size() >= 1"
        , "    && ("
        , "      data!=null && 'y' in data && data['y'] is list && data['y'].size() > 0 && is____Y(data['y'][0], prev!=null && 'y' in prev && prev['y'] is list && prev['y'].size() > 0 ? prev['y'][0] : null)"
        , "    )"
        , "    && ("
        , "      !(data!=null && 'y' in data && data['y'] is list && data['y'].size() > 1) ||  data['y'][1] == null  || is____Y(data['y'][1], prev!=null && 'y' in prev && prev['y'] is list && prev['y'].size() > 1 ? prev['y'][1] : null)"
        , "    )"
        , "  );"
        , "}"
        ]

    it "generates a n-tuple check of definite size" $
      g "type X = { x: string[2] }"
      `shouldBe` ru
        [ "function is____X(data, prev) {"
        , "  return data.keys().hasAll(['x'])"
        , "  && data.keys().hasOnly(['x'])"
        , "  && ( data['x'] is list && data['x'].size() <= 2 && data['x'].size() >= 0"
        , "    && ("
        , "      !(data!=null && 'x' in data && data['x'] is list && data['x'].size() > 0) ||  data['x'][0] == null  || data['x'][0] is string"
        , "    )"
        , "    && ("
        , "      !(data!=null && 'x' in data && data['x'] is list && data['x'].size() > 1) ||  data['x'][1] == null  || data['x'][1] is string"
        , "    )"
        , "  );"
        , "}"
        ]


    
    
    it "creates a correct check for empty type object" $
      g "type X = {}" `shouldBe` ru
        [ "function is____X(data, prev) {"
        , "  return data.keys().hasOnly([])"
        , "  ;"
        , "}"
        ]
        

    it "generates a path with a type" $
      g "match x is X {}" `shouldBe` ru
        [ "match /x {"
        , "  function is______PathType(data, prev) {"
        , "    return is____X(data, prev!=null ? prev : null);"
        , "  }"
        , "  allow write: if is______PathType(request.resource.data, resource==null ? null : resource.data);"
        , "}"
        ]

    it "generates a path with a type and custom write condition" $
      g "match x is X { allow create: if true; }" `shouldBe` ru
        [ "match /x {"
        , "  function is______PathType(data, prev) {"
        , "    return is____X(data, prev!=null ? prev : null);"
        , "  }"
        , "  allow create: if is______PathType(request.resource.data, resource==null ? null : resource.data) && (true);"
        , "}\n"
        ]

    it "generates validations for types" $
      gu
      [ "type X = {"
      , "a: string"
      , "allow create, delete: if prev['a'] == data['a'] || 1==1"
      , "allow update, delete: 2==2 && 3==3"
      , "allow write: true"
      , "}"
      ] `shouldBe` ru
        [ "function is____X(data, prev) {"
        , "  return ("
        , "    ( request.method != 'create' || ( prev['a'] == data['a'] || 1 == 1 ) )"
        , "    && ( request.method != 'delete' || ( prev['a'] == data['a'] || 1 == 1 ) )"
        , "    && ( request.method != 'update' || ( 2 == 2 && 3 == 3 ) )"
        , "    && ( request.method != 'delete' || ( 2 == 2 && 3 == 3 ) )"
        , "    && ( request.method != 'create' || ( true ) )"
        , "    && ( request.method != 'update' || ( true ) )"
        , "    && ( request.method != 'delete' || ( true ) )"
        , "  ) && data.keys().hasAll(['a'])"
        , "  && data.keys().hasOnly(['a'])"
        , "  && data['a'] is string;"
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


  describe "Top Level Variables" $ do
    it "generates rule version 2 if not specified" $
      gv "type X = any" `shouldBe` (trim . unlines) [
        "Right rules_version = '2';",
        "service cloud.firestore {",
        "  match /databases/{database}/documents {",
        "    function is____X(data, prev) {\n      return true;\n    }",
        "  }",
        "}"
      ]

  describe "Literal" $ do
    it "handle a boolean" $
      g "type X = { a: true }" `shouldBe` "Right function is____X(data, prev) {\n  return data.keys().hasAll(['a'])\n  && data.keys().hasOnly(['a'])\n  && data['a'] == true;\n}" 
    it "handle a string" $ do
      g "type X = { a: 'me' | \"you\" }" `shouldBe` "Right function is____X(data, prev) {\n  return data.keys().hasAll(['a'])\n  && data.keys().hasOnly(['a'])\n  && (\n    data['a'] == 'me'\n    || data['a'] == \"you\"\n  );\n}"
    it "handle a float" $
      g "type X = { a: 123.3 }" `shouldBe` "Right function is____X(data, prev) {\n  return data.keys().hasAll(['a'])\n  && data.keys().hasOnly(['a'])\n  && data['a'] == 123.3;\n}" 

  describe "Primitive types" $ do
    it "handles any nested in object" $
      g "type X = { a: any }" `shouldBe` "Right function is____X(data, prev) {\n  return data.keys().hasAll(['a'])\n  && data.keys().hasOnly(['a'])\n  && true;\n}" 
    it "handles any as top-level type" $
      g "type X = any" `shouldBe` "Right function is____X(data, prev) {\n  return true;\n}" 
      
    
  describe "Smoke Test" $ do
    it "passes" $ do
      ward <- readFile "examples/smoke-test.ward"
      g ward `shouldSatisfy` (not . startsWith "Left")
