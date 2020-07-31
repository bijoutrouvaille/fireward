module TSGeneratorSpec (main, spec, stdTypes) where

import Parser (trim)
import TSGenerator

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
g = showE . TSGenerator.generate
gt z = (\x->trace (showN x) x) (g z)
gu = g . trim . unlines
r = ("Right " ++) . repA
ru = r . trim . unlines
timestamp = "null|Date|WardTimestamp|WardFieldValue"
                 

spec :: Spec
spec = do
  describe "Typescript Generator" $ do
    it "generates a simple thing" $
      g "type X = Y | {a:int}"
      `shouldBe` ru
      [ stdTypes ++ "export type X = Y | {"
      , "  a: number"
      , "}"
      ]
    it "generates a simple array" $ do
      g "type X = {a:string[]}" `shouldBe` ru
        [ stdTypes ++ "export type X = {"
        , "  a: string[]"
        , "}"
        ]
    it "generates a 2-tuple array" $ do
      g "type X = {a:string[2]}" `shouldBe` ru
        [ stdTypes ++ "export type X = {"
        , "  a: [string?, string?]"
        , "}"
        ]
    it "generates a 3-tuple" $ do
      g "type X = {a: [string, Z, {b: 1}]}" `shouldBe` ru
        [ stdTypes ++ "export type X = {"
        , "  a: [string, Z, {\n  b: 1\n}]"
        , "}"
        ]
    it "generates a grouped array" $ do
      g "type X = (string | float)[]" `shouldBe` ru
        [ stdTypes ++ "export type X = (string | number)[]" ]
    it "generates the any type" $ do
      g "type X = {a:any}" `shouldBe` ru
        [ stdTypes ++ "export type X = {\n  a: any\n}" ]
    it "eats line comments" $ 
      gu
      [ "type X = { // type X"
      , "// a line of comment"
      , "  a: string // a is a string"
      , "}"
      ] 
      `shouldBe` ru
      [ stdTypes ++ "export type X = {"
      , "  a: string"
      , "}"
      ];

    it "generates a compound thing" $
      gu 
      [ "type X = Y | {a:int}"
      , "type Compound = {"
      , "  a: timestamp"
      , "  b: {"
      , "    ba: int"
      , "    bb: {"
      , "      bba: string"
      , "    }"
      , "  }"
      , "}"
      ]
      `shouldBe` ru
      [ stdTypes ++ "export type X = Y | {"
      , "  a: number"
      , "}"
      , "export type Compound = {"
      , "  a: " ++ timestamp
      , "  b: {"
      , "    ba: number"
      , "    bb: {"
      , "      bba: string"
      , "    }"
      , "  }"
      , "}"
      ]

