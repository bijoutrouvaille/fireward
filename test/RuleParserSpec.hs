module RuleParserSpec (main, spec) where

import Parser
import RuleParser

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
  describe "rule parser" $ do
    it "parses a field" $
      apply _field "zoo: null" `shouldBe` [(Field True "zoo" [TypeNameRef "null"],"")]
    it "parses a type" $
      apply _typeDef "{ zoo: null }" `shouldBe` [(TypeDef [Field True "zoo" [TypeNameRef "null"]],"")]
    it "parses a simple type" $
      parseRules "type Hor = { zoo: Null }" `shouldBe` [([
          TopLevelType "Hor" [InlineTypeRef (TypeDef [Field True "zoo" [TypeNameRef "Null"]])]
      ],"")]
    it "parses two simple types" $
      parseRules (unlines [ "type A = {a:Null}"
                          , "type B = {b:Null}"
                          ]) `shouldBe` [([
                            TopLevelType "A" [InlineTypeRef (TypeDef [Field True "a" [TypeNameRef "Null"]])], 
                            TopLevelType "B" [InlineTypeRef (TypeDef [Field True "b" [TypeNameRef "Null"]])]
                          ], "")]
    it "parses a pathStatic in pathParts" $
      apply _pathParts "hello" `shouldBe` [([PathPartStatic "hello"],"")]
    it "parses a pathVar in pathParts" $
      apply _pathParts "{hello}" `shouldBe` [([PathPartVar "hello"],"")]
    it "parses a pathWild in pathParts" $
      apply _pathParts "{hello=**}" `shouldBe` [([PathPartWild "hello"],"")]
    it "parses a pathStatic and Var in pathParts" $
      apply _pathParts "hello/{world=**}" `shouldBe` [([
          PathPartStatic "hello",
          PathPartWild "world"
      ],"")]
    it "parses a pathStatic, Wild and Var in pathParts" $
      apply _pathParts "hello/{pretty}/{world=**}" `shouldBe` [([
          PathPartStatic "hello",
          PathPartVar "pretty",
          PathPartWild "world"
      ],"")]

    it "parses a full path" $
      apply _path (unlines
      [ "match /hello/{pretty}/{world=**} is Rough {"
      , "}"
      ]) `shouldBe` [(PathDef [
          PathPartStatic "hello",
          PathPartVar "pretty",
          PathPartWild "world"
      ] [ TypeNameRef "Rough" ] [], "\n")]
    -- data PathDirective = PathDirective [PathOp] PathCondition
    it "parses a path directive" $
      apply _pathDir "allow read: if 1<3 && true;" `shouldBe` [
        (PathDirective [ "read" ] "1<3 && true", "")
      ]


    it "parses a path without className" $
      parseRules (unlines 
                 [ "match /stat/{var}/{wild=**} {"
                 , "  allow read: if 1>2 && 3<4;"
                 , "}"
                 ]) `shouldBe` [([
                   TopLevelPath (PathDef [
                     PathPartStatic "stat",
                     PathPartVar "var",
                     PathPartWild "wild"
                   ] [] [PathBodyDir (PathDirective ["read"] "1>2 && 3<4")])
                 ],"")]
    it "parses a field and a path" $
      parseRules (unlines 
                 [ "type A = {a: String}"
                 , "match /stat/{var}/{wild=**} {"
                 , "  allow read: if 1>2 && 3<4;"
                 , "}"
                 ]) `shouldBe` [([
                  TopLevelType "A" [InlineTypeRef (TypeDef [Field True "a" [TypeNameRef "String"]])],
                  TopLevelPath (PathDef [
                    PathPartStatic "stat",
                    PathPartVar "var",
                    PathPartWild "wild" 
                  ] [] [
                    PathBodyDir (PathDirective ["read"] "1>2 && 3<4")
                  ])
                 ],"")]

    it "parses a one-line path" $
      parseRules "match a/{x=**} is X {allow create: if true;}"
        `shouldBe` [([TopLevelPath (PathDef [
          PathPartStatic "a", PathPartWild "x"
        ] [ TypeNameRef "X" ] [
          PathBodyDir (PathDirective ["create"] "true")
        ])], "")]
  describe "escape" $ do
    it "detects escaped chars" $
      apply (escape '\'') "\\''" `shouldBe` [("\\'", "'")]
    it "detects many escaped chars" $
      apply (many $ escape '\'') "\\'\\'x" `shouldBe` [(["\\'", "\\'"], "x")]
    it "detects an escaped char or a arbitrary" $
      apply (escape '\'' <|> _const "f") "\\'x" `shouldBe` [("\\'", "x")]
    it "detects many of an escaped char or arbitrary" $
      apply (many $ escape '\'' <|> _const "f") "\\'f\\'fx" `shouldBe` 
        [(["\\'", "f", "\\'", "f"], "x")]
    it "detects many of an escaped char or many sats" $
      apply (many $ escape '\'' <|> (:[]) <$> (sat (/='0'))) "\\'f\\'fx" `shouldBe` 
        [(["\\'", "f", "\\'", "f", "x"], "")]
        
    it "parses a string" $
      apply _string "'hello world' + 3"
        `shouldBe` [("'hello world'", " + 3")]
    it "parses a string with escaped quotes" $
      apply _string "'hello \\'world' + 3"
        `shouldBe` [("'hello \\'world'", " + 3")]
    it "parses a one-line function" $ 
      parseRules "function abc(h) { return x.y || b }"
      `shouldBe`
      [([TopLevelFunc (FuncDef "abc" ["h"] "x.y || b")],"")]
    it "parses a multiline function" $
      parseRules (unlines [
        "function abc(h) { ",
        "  return x.y || b",
        "}"
      ])
      `shouldBe`
      [([TopLevelFunc (FuncDef "abc" ["h"] "x.y || b")],"")]
    it "parses a complex type" $
      parseRules (unlines [
        "type Zxx = Null | { ",
        "  one: X,",
        "  two: {three: Z, four: P|X}",
        "}"
      ])
      `shouldBe`
      [([ TopLevelType "Zxx" [TypeNameRef "Null", InlineTypeRef (TypeDef [
        Field True "one" [TypeNameRef "X"],
        Field True "two" [InlineTypeRef (TypeDef [
          Field True "three" [TypeNameRef "Z"],
          Field True "four" [TypeNameRef "P",TypeNameRef "X"]
        ])]
      ])]],"")]
    it "parses a function a type and a path" $
      parseRules (unlines [
        "type Zxx = { ",
        "  one: X,",
        "  two: Y",
        "}",
        
        "function abc(h) { ",
        "  return x.y || b",
        "}"
      ])
      `shouldBe`
      [([ TopLevelType "Zxx" [InlineTypeRef (TypeDef [Field True "one" [TypeNameRef "X"],Field True "two" [TypeNameRef "Y"]])],
          TopLevelFunc (FuncDef "abc" ["h"] "x.y || b")
        ],"")]
    it "parses a complex path" $
      parseRules (unlines [
        "match /x is A {",
        "  match /y is B {",
        "    allow read, write: if true;",
        "    allow create, write: if false;",
        "    function qqq(a,b,c) {",
        "      return 123",
        "    }",
        "  }",
        "}"
      ]) `shouldBe` [([ TopLevelPath (PathDef [PathPartStatic "x"] [ TypeNameRef "A" ] [
        PathBodyPath (PathDef [PathPartStatic "y"] [ TypeNameRef "B" ] [
          PathBodyDir (PathDirective ["read","write"] "true"),
          PathBodyDir (PathDirective ["create","write"] "false"),
          PathBodyFunc (FuncDef "qqq" ["a","b","c"] "123")])
        ])
      ],"")]


