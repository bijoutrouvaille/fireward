module TSGeneratorSpec (main, spec) where

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
showE (Left (Error loc x)) = "Left " ++ repA x
g = showE . TSGenerator.generate
gt z = (\x->trace (showN x) x) (g z)
gu = g . trim . unlines
r = ("Right " ++) . repA
ru = r . trim . unlines

spec :: Spec
spec = do
  describe "Typescript Generator" $ do
    it "generates a simple thing" $
      g "type X = Y | {a:int}"
      `shouldBe` ru
      [ "type X = Y | {"
      , "  a: number"
      , "}"
      ]

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
      [ "type X = Y | {"
      , "  a: number"
      , "}"
      , "type Compound = {"
      , "  a: number"
      , "  b: {"
      , "    ba: number"
      , "    bb: {"
      , "      bba: string"
      , "    }"
      , "  }"
      , "}"
      ]

