module TSGenerator
( Error (..)
, loc
, generate
) where


import Parser
import RuleParser

import Loc (loc, Loc(..))
import Error (Error(..))

import Data.List (findIndices, intercalate, stripPrefix)
import Data.Char (toUpper)
import Control.Monad (ap)

capitalize "" = ""
capitalize (c:cs) = (toUpper c) : cs

joinLines = intercalate "\n"

indent n = take n $ repeat ' '
indentBy n = (indent n ++)

block :: Int -> [String] -> String
block ind items = joinLines
  [ "{"
  , joinLines $ (indent (ind + 2) ++) <$> items
  , indent ind ++ "}"
  ]


natives = 
  [ ("int", "number")
  , ("float", "number")
  , ("timestamp", "number")
  ]
fork f g a = (f a) (g a)
typeBlock :: Int -> TypeDef -> String
typeBlock ind (TypeDef fields) = block ind $
  f <$> fields
  where
    f (Field r name refs) = name ++ ": " ++ typeRefList (ind + 2) refs
typeRefList :: Int -> [TypeRef] -> String
typeRefList ind refs = 
  trim .  intercalate " | " $ ref <$> refs
  where
    convertToNative = flip maybe id <*> flip lookup natives
    -- tldr: q = f <*> g === ap f g === q x = (f x) (g x).
    -- see the full explanation in docs/func-modad.md
    ref (TypeNameRef name) = convertToNative name
    ref (InlineTypeRef def) = typeBlock ind def

topLevelType name refs = "export type "++name++" = "++typeRefList 0 refs

gen :: [TopLevel] -> Either Error String
gen tops = result where
  result = Right $ joinLines strings
  strings = g <$> tops 
  g (TopLevelType name refs) = topLevelType name refs
  g _ = ""

generate :: String -> Either Error String
generate s = result $ parseRules s where
  result [] = Left $ Error Nothing "Indeterministic nonesense."
  result ((tops, ""):_) = gen tops
  result ((_, rem):_) = Left $ Error (loc s rem) "Could not parse"

