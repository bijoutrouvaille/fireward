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
  , ("timestamp", "null|Date|WardTimestamp|WardFieldValue")
  , ("latlng", "WardGeoPoint")
  , ("bool", "boolean")
  , ("null", "null")
  , ("map", "Record<string, unknown>")
  ]



maxTuples = 12
tupleX n opt = "[" ++ (intercalate ", " ["T" ++ if opt then "?" else "" | j <- [0..n-1]]) ++ "]"
maxTupleX n = "export type ArrayMax"++show n++"<T> = "  ++ (tupleX n True) 
someTuple = "export type SomeTuple<T> = " ++ intercalate " | " [ "ArrayMax" ++ show j ++ "<T> "| j <- [1..maxTuples-1]]
funcMaxArray = ts ++ "\nexport function toArrayMax(n:number, arr:any[]) { return arr.slice(0,n) }" 
  where ts = intercalate "\n" ["export function toArrayMax<T>(n: "++show n++", arr:T[]):ArrayMax"++show n++"<T>" | n <- [1..maxTuples-1]]
fieldValueType = "export type WardFieldValue = { isEqual: (other: WardFieldValue) => boolean };"
timestampType = "export type WardTimestamp = {seconds: number, nanoseconds: number, toDate: ()=>Date, isEqual: (other: WardFieldValue)=>boolean, toMillis: ()=>number};"
timestampTypeCheck = "export function isTimestamp(v: any): v is WardTimestamp { return !!v && (typeof v=='object') && !!v.toDate && !!v.toMillis && (typeof v.nanoseconds=='number') && (typeof v.seconds=='number')};"
geoPointType = "export type WardGeoPoint = { latitude: number, longitude: number, isEqual: (other: WardGeoPoint)=>boolean }"
geoPointTypeCheck = "export function isGeoPoint(v: any): v is WardGeoPoint {  return !!v && (typeof v=='object') && (typeof v.isEqual=='function')  && (typeof v.latitude=='number') && (typeof v.longitude=='number') };"
stdTuples = intercalate "\n\n" [ maxTupleX n | n <- [1..maxTuples] ]

stdTypes = (intercalate "\n" 
           [ fieldValueType
           , timestampType
           , timestampTypeCheck
           , geoPointType
           , geoPointTypeCheck
           ]) ++ "\n\n"

fork f g a = (f a) (g a)
typeBlock :: Int -> TypeDef -> String
typeBlock ind (TypeDef fields _) = block ind $
  f <$> fields
  where
    f (Field r name refs c) = name ++ (if r then "" else "?") ++ ": " ++ typeRefList (ind + 2) refs
typeRefList :: Int -> [TypeRef] -> String
typeRefList ind refs = 
  trim .  intercalate " | " $ ref <$> refs
  where
    -- tldr: q = f <*> g === ap f g === q x = (f x) (g x).
    convertToNative = flip maybe id <*> flip lookup natives
    ref :: TypeRef -> String
    ref (LiteralTypeRef value) = value
    ref (ListTypeRef r) = ref r ++ "[]"
    ref (GroupedTypeRef refs) = "(" ++ typeRefList 0 refs ++ ")"
    ref (TupleTypeRef elems) = "[" ++ intercalate ", " (fmap fromTupleElem elems) ++ "]"
    ref (TypeNameRef name ) = convertToNative name
    ref (InlineTypeDef def) = typeBlock ind def


    fromTupleElem (req, refs) = 
      if req then typeRefList 0 refs
             else if length refs == 1
             then typeRefList 0 refs ++ "?"
             else "(" ++ typeRefList 0 refs ++ ")?"
                           

topLevelType name refs = "export type " ++ name ++ " = " ++ typeRefList 0 refs

gen :: [TopLevel] -> Either String String
gen tops = result where
  result = Right $ stdTypes ++ joinLines strings
  strings = g <$> tops 
  g (TopLevelType name refs) = topLevelType name refs
  g _ = ""


generate :: String -> Either String String
generate s = result $ parseRules s where
  result (Left Nothing) = Left ("Unexpected parser error.")
  result (Left (Just (e,l,c))) = Left (e ++ "\n  on " ++printLoc l c)
  result (Right (tops, "", _, _)) = gen tops
  result (Right (tops, unparsed, l, c)) = Left ("Unexpected input on " ++ printLoc l c)
  printLoc l c = "line " ++ show (l+1) ++", column "++show (c+1)
