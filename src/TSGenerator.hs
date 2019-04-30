module TSGenerator
( Error (..)
, loc
, generate
, stdTypes
) where


import Parser
import RuleParser

import Loc (loc, Loc(..))
import Error (Error(..))

import Data.List (findIndices, intersperse, intercalate, stripPrefix)
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
  , ("timestamp", "{seconds: number, nanoseconds: number}|{isEqual: (other: any)=>boolean}")
  , ("bool", "boolean")
  , ("null", "null")
  ]



maxTuples = 12
tupleX n opt = "[" ++ (intercalate ", " ["T" ++ if opt then "?" else "" | j <- [0..n-1]]) ++ "]"
maxTupleX n = "export type ArrayMax"++show n++"<T> = "  ++ (tupleX n True) 
someTuple = "export type SomeTuple<T> = " ++ intercalate " | " [ "ArrayMax" ++ show j ++ "<T> "| j <- [1..maxTuples-1]]
funcMaxArray = ts ++ "\nexport function toArrayMax(n:number, arr:any[]) { return arr.slice(0,n) }" 
  where ts = intercalate "\n" ["export function toArrayMax<T>(n: "++show n++", arr:T[]):ArrayMax"++show n++"<T>" | n <- [1..maxTuples-1]]

stdTuples = intercalate "\n\n" [ maxTupleX n | n <- [1..maxTuples] ]

stdTypes = (intercalate "\n" 
           [ stdTuples 
           , someTuple
           , funcMaxArray
           ]) ++ "\n\n"

fork f g a = (f a) (g a)
typeBlock :: Int -> TypeDef -> String
typeBlock ind (TypeDef fields) = block ind $
  f <$> fields
  where
    f (Field r name refs c) = name ++ (if r then "" else "?") ++ ": " ++ typeRefList (ind + 2) refs
typeRefList :: Int -> [TypeRef] -> String
typeRefList ind refs = 
  trim .  intercalate " | " $ ref <$> refs
  where
    convertToNative = flip maybe id <*> flip lookup natives
    -- tldr: q = f <*> g === ap f g === q x = (f x) (g x).
    -- see the full explanation in docs/func-monad.md
    ref (TypeNameRef name Nothing) = convertToNative name
    ref (TypeNameRef name (Just size)) = if size==0 || size > 12
                                            then convertToNative name ++ "[]"
                                            else "ArrayMax"++show size++"<"++name++">"
                                            -- else intercalate " | " [ "["++intercalate ", " [ name | j <- [0..i]]++"]"
                                            --        | i <- [0..size-1]
                                                 -- ]
    ref (InlineTypeRef def) = typeBlock ind def

topLevelType name refs = "export type "++name++" = "++typeRefList 0 refs

gen :: [TopLevel] -> Either String String
gen tops = result where
  result = Right $ stdTypes ++ joinLines strings
  strings = g <$> tops 
  g (TopLevelType name refs) = topLevelType name refs
  g _ = ""

generate :: String -> Either String String
generate s = result $ parseRules s where
  result (Left (e,l,c)) = Left (e ++ "\n  on " ++printLoc l c) -- $ Error Nothing "Indeterministic nonesense."
  result (Right (tops, "", _, _)) = gen tops
  result (Right (tops, unparsed, l, c)) = Left ("Unexpected input at " ++ printLoc l c)
  -- result ((tops, ""):_) = gen tops
  -- result ((_, rem):_) = Left $ Error (loc s rem) "Could not parse"
  -- printLoc l c = show l ++":"++show c
  printLoc l c = "line " ++ show (l+1) ++", column "++show (c+1)

