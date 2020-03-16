module RuleParserSpec (main, spec) where

import Parser
import RuleParser

import Data.Char (isDigit)
import Control.Applicative
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Debug.Trace (trace)


_parse s = res (parseRules s) 
  where res (Right (x,v, _, _)) = Right (x,v)
        res (Left x) = Left x

_apply p s = res (apply p s) 
  where res (Right (x, u, l, c)) = Right (x, u)
        res (Left x) = Left x

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rule parser" $ do
    it "parses a field" $
      _apply _field "zoo: null" `shouldBe` Right (Field True "zoo" [TypeNameRef "null"] False,"")
    it "parses a type" $
      _apply _typeDef "{ zoo: null }" `shouldBe` Right (TypeDef [Field True "zoo" [TypeNameRef "null"] False] [],"")
    it "parses a simple type" $
      _parse "type Hor = { zoo: Null }" `shouldBe` Right ([
          TopLevelType "Hor" [InlineTypeDef (TypeDef [Field True "zoo" [TypeNameRef "Null"] False] [])]
      ],"")
    it "parses two simple types" $
      _parse (unlines [ "type A = {a:Null}"
                          , "type B = {b:Null}"
                          ]) `shouldBe` Right ([
                            TopLevelType "A" [InlineTypeDef (TypeDef [Field True "a" [TypeNameRef "Null"] False] [])], 
                            TopLevelType "B" [InlineTypeDef (TypeDef [Field True "b" [TypeNameRef "Null"] False] [])]
                          ], "")
    it "parses a type with all optional fields" $
      _parse "type X = { x?: string }" `shouldBe` Right ([TopLevelType "X" [
        InlineTypeDef (TypeDef [Field {required = False, fieldName = "x", typeRefs = [TypeNameRef "string"], constant = False}] [])]]
      ,"")
    it "parses a pathStatic in pathParts" $
      _apply _pathParts "hello" `shouldBe` Right ([PathPartStatic "hello"],"")
    it "parses a pathVar in pathParts" $
      _apply _pathParts "{hello}" `shouldBe` Right ([PathPartVar "hello"],"")
    it "parses a pathWild in pathParts" $
      _apply _pathParts "{hello=**}" `shouldBe` Right ([PathPartWild "hello"],"")
    it "parses a pathStatic and Var in pathParts" $
      _apply _pathParts "hello/{world=**}" `shouldBe` Right ([
          PathPartStatic "hello",
          PathPartWild "world"
      ],"")
    it "parses a pathStatic, Wild and Var in pathParts" $
      _apply _pathParts "hello/{pretty}/{world=**}" `shouldBe` Right ([
          PathPartStatic "hello",
          PathPartVar "pretty",
          PathPartWild "world"
      ],"")

    it "parses a full path" $
      _apply _path (unlines
      [ "match /hello/{pretty}/{world=**} is Rough {"
      , "}"
      ]) `shouldBe` Right (PathDef [
          PathPartStatic "hello",
          PathPartVar "pretty",
          PathPartWild "world"
      ] [ TypeNameRef "Rough"] [], "\n")
    it "parses a path directive" $
      _apply _pathDir "allow read: if 1<3 && true;" `shouldBe` 
        Right (PathDirective [ "read" ] "1 < 3 && true", "")
      


    it "parses a path without className" $
      _parse (unlines 
                 [ "match /stat/{var}/{wild=**} {"
                 , "  allow read: if 1>2 && 3<4;"
                 , "}"
                 ]) `shouldBe` Right ([
                   TopLevelPath (PathDef [
                     PathPartStatic "stat",
                     PathPartVar "var",
                     PathPartWild "wild"
                   ] [] [PathBodyDir (PathDirective ["read"] "1 > 2 && 3 < 4")])
                 ],"")
    it "parses a field and a path" $
      _parse (unlines 
                 [ "type A = {a: String}"
                 , "match /stat/{var}/{wild=**} {"
                 , "  allow read: if 1>2 && 3<4;"
                 , "}"
                 ]) `shouldBe` Right ([
                  TopLevelType "A" [InlineTypeDef (TypeDef [Field True "a" [TypeNameRef "String"] False] [])],
                  TopLevelPath (PathDef [
                    PathPartStatic "stat",
                    PathPartVar "var",
                    PathPartWild "wild" 
                  ] [] [
                    PathBodyDir (PathDirective ["read"] "1 > 2 && 3 < 4")
                  ])
                 ],"")

    it "parses a one-line path" $
      _parse "match a/{x=**} is X {allow create: if true;}"
        `shouldBe` Right ([TopLevelPath (PathDef [
          PathPartStatic "a", PathPartWild "x"
        ] [ TypeNameRef "X"] [
          PathBodyDir (PathDirective ["create"] "true")
        ])], "")
    it "fails on just the type keyword" $ do
      _parse "type" `shouldBe` failure ("type name missing", 0, 4)
    it "fail when type definition is missing =" $
      _parse "type X {}" `shouldBe` failure ("missing `=` after type name", 0, 6)
    it "fails when there is nothing after assignment" $ do
      _parse "type X =" `shouldBe` failure ("type `X` is missing definition", 0, 8)
    it "allows a semicolon after type definition" $ do
      _parse "type X = string;" `shouldBe` Right ([TopLevelType "X" [TypeNameRef "string" ]], "")

    it "parses regex" $ do
      _parse "match /x/x { allow write: if name.match('test'); }" `shouldBe` Right ([TopLevelPath (PathDef [PathPartStatic "x",PathPartStatic "x"] [] [PathBodyDir (PathDirective ["write"] "name.match('test')")])],"")
    it "parses a regex call with other condition" $ do
      _parse "match /f/{x} {\n  allow write: if 1+2==2 && x.match('^he..o'); \n}" `shouldBe` 
        Right ([TopLevelPath (PathDef [PathPartStatic "f",PathPartVar "x"] [] [PathBodyDir (PathDirective ["write"] "1 + 2 == 2 && x.match('^he..o')")])],"")

    it "fails on a property without a definition" $ do
      _parse "type X = {fff}" `shouldBe` failure ("type `X` is missing definition", 0, 8)
    it "fails when a field lacks a type" $ do
      _parse "type X = {a: }" `shouldBe` failure ("field `a` lacks a type", 0, 12) 

    it "fails when a function is missing a name" $ do
      _parse "function (abc) {}" `shouldBe` failure ("missing function name", 0, 8)
    it "fails when a function is missing parameter parens" $ do
      _parse "function z {}" `shouldBe` failure ("function `z` is missing the parameter list", 0, 10)
    it "fails when function is missing opening {" $ do
      _parse "function z() }" `shouldBe` failure ("function `z` is missing an opening `{`",0,12)
    it "fails when function is missing closing }" $ do
      _parse "function z() {" `shouldBe` failure ("function `z` is missing a closing `}`",0,14)
    it "fails when function body is missing" $ do
      _parse "function z() {  \n  }" `shouldBe` failure ("function `z` is missing a body", 1,3)
    it "fails when path has an `is` but no type" $ do
      _parse "match /x is {}" `shouldBe` failure ("expected a `{`", 0, 14)

    it "parses a string" $ do
      (_apply _string "\"abc\"") `shouldBe` Right ("\"abc\"", "")
    it "parses a string with semicolons" $ do
      (_apply _string "\"abc;\"") `shouldBe` Right ("\"abc;\"", "")
    it "parses a string with foreign quotes" $ do
      (_apply _string "\"abc'\"") `shouldBe` Right ("\"abc'\"", "")
    it "parses a string with own but escaped quotes" $ do
      (_apply _string "\"abc\\\"\"") `shouldBe` Right ("\"abc\\\"\"", "")

    it "allows for strings with semicolons in rule conditions" $ do
      _parse "match /x { allow read: if x==\"123;\" && y-3==4 ; }" `shouldBe` Right ([TopLevelPath (PathDef [PathPartStatic "x"] [] [PathBodyDir (PathDirective ["read"] "x == \"123;\" && y - 3 == 4")])],"")

    it "parses multiple directives without semicolon separators" $ do
      let r = unlines
              [ "match /x {"
              , "  allow read: true"
              , "  function z() { true }"
              , "  allow create: false"
              , "  match /q {}"
              , "}" 
              ]
      _parse r `shouldBe` Right ([ TopLevelPath (PathDef [PathPartStatic "x"] [] 
        [ PathBodyDir (PathDirective ["read"] "true")
        , PathBodyFunc (FuncDef "z" [] [] "true")
        , PathBodyDir (PathDirective ["create"] "false")
        , PathBodyPath (PathDef [PathPartStatic "q"] [] [])
        ])], "")

  describe "escape" $ do
    it "detects escaped chars" $
      _apply (escape '\'') "\\''" `shouldBe` Right ("\\'", "'")
    it "detects many escaped chars" $
      _apply (many $ escape '\'') "\\'\\'x" `shouldBe` Right (["\\'", "\\'"], "x")
    it "detects an escaped char or a arbitrary" $
      _apply (escape '\'' <|> _const "f") "\\'x" `shouldBe` Right ("\\'", "x")
    it "detects many of an escaped char or arbitrary" $
      _apply (many $ escape '\'' <|> _const "f") "\\'f\\'fx" `shouldBe` 
        Right (["\\'", "f", "\\'", "f"], "x")
    it "detects many of an escaped char or many sats" $
      _apply (many $ escape '\'' <|> (:[]) <$> (sat (/='0'))) "\\'f\\'fx" `shouldBe` 
        Right (["\\'", "f", "\\'", "f", "x"], "")
        
    it "parses a string" $
      _apply _string "'hello world' + 3"
        `shouldBe` Right ("'hello world'", " + 3")
    it "parses a string with escaped quotes" $
      _apply _string "'hello \\'world' + 3"
        `shouldBe` Right ("'hello \\'world'", " + 3")
    it "parses a one-line function" $ 
      _parse "function abc(h) { return x.y || b }"
      `shouldBe`
      Right ([TopLevelFunc (FuncDef "abc" ["h"] [] "x.y || b")],"")
    it "parses a multiline function" $
      _parse (unlines [
        "function abc(h) { ",
        "  return x.y || b",
        "}"
      ])
      `shouldBe`
      Right ([TopLevelFunc (FuncDef "abc" ["h"] [] "x.y || b")],"")
    it "parses a complex type" $
      _parse (unlines [
        "type Zxx = Null | { ",
        "  one: X,",
        "  two: {three: Z, four: P|X}",
        "}"
      ])
      `shouldBe`
      Right ([ TopLevelType "Zxx" [TypeNameRef "Null" , InlineTypeDef (TypeDef [
        Field True "one" [TypeNameRef "X" ] False,
        Field True "two" [InlineTypeDef (TypeDef [
          Field True "three" [TypeNameRef "Z" ] False,
          Field True "four" [TypeNameRef "P" , TypeNameRef "X" ] False
        ] [])] False
      ] [])]],"")
    it "parses a function that returns a string" $ 
      _parse "function q(a) { return 'p' }" `shouldBe` Right ([TopLevelFunc (FuncDef "q" ["a"] [] "'p'")], "")

    it "parses a function a type and a path" $
      _parse (unlines [
        "type Zxx = { ",
        "  one: X,",
        "  two: Y",
        "}",
        
        "function abc(h) { ",
        "  return x.y || b",
        "}"
      ])
      `shouldBe`
      Right ([ TopLevelType "Zxx" [InlineTypeDef (TypeDef [Field True "one" [TypeNameRef "X" ] False, Field True "two" [TypeNameRef "Y" ] False] [])],
          TopLevelFunc (FuncDef "abc" ["h"] [] "x.y || b")
        ],"")
    it "parses a complex path" $
      _parse (unlines [
        "match /x is A {",
        "  match /y is B {",
        "    allow read, write: if true;",
        "    allow create, write: if false;",
        "    function qqq(a,b,c) {",
        "      return 123",
        "    }",
        "  }",
        "}"
      ]) `shouldBe` Right ([ TopLevelPath (PathDef [PathPartStatic "x"] [ TypeNameRef "A" ] [
        PathBodyPath (PathDef [PathPartStatic "y"] [ TypeNameRef "B" ] [
          PathBodyDir (PathDirective ["read","write"] "true"),
          PathBodyDir (PathDirective ["create","write"] "false"),
          PathBodyFunc (FuncDef "qqq" ["a","b","c"] [] "123")])
        ])
      ],"")
  describe "_topLevelOptVar" $ do
    it "parses" $
      _parse "fff = '2'" `shouldBe` Right ([ TopLevelOpt "fff" "'2'"], "")



