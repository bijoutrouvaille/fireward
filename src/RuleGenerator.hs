module RuleGenerator (
  loc,
  generate
) where

import Parser
import RuleParser
import Data.List (findIndices, intercalate, stripPrefix)
import Data.Char (toUpper)

data Loc = Loc Int Int
         deriving (Show, Eq)
data Error = Error (Maybe Loc) String
         deriving (Show, Eq)

loc :: [String] -> [String] -> Loc
loc _ [] = Loc 0 0
loc [] _ = Loc 0 0
loc (s:reverseSourceLines) (u:reverseUnparsedLines) = 
  if s == u
     then addLine $ loc reverseSourceLines reverseUnparsedLines
     else Loc 0 (length $ stripPrefix (reverse u) (reverse s))
  where
    addLine (Loc l c) = Loc (l+1) c
  
getOr l n f = if length l < n 
                 then Right (l!!n) 
                 else Left f

capitalize "" = ""
capitalize (c:cs) = (toUpper c) : cs

joinLines = intercalate "\n"

indent n = take (n*2) $ repeat ' '

block :: Int -> [String] -> String
block ind items = joinLines
  [ "{"
  , joinLines $ (indent (ind + 1) ++) <$> items
  , indent ind ++ "}"
  ]

surround :: String -> String -> String -> String
surround b e s = concat [b,s,e]

generate :: String -> Either Error String
generate source = if length allResults == 0 
                     then Left $ Error Nothing "No results at all"
                     else allResults !! 0
  where
    allResults :: [Either Error String]
    allResults = fmap q tree
    tree :: [([TopLevel], String)]
    tree = parseRules source
    q :: ([TopLevel], String) -> Either Error String
    q (tops, unparsed) = if length unparsed > 0
                            then Left $ Error (Just $ loc (lines source) (lines unparsed)) "Could not parse."
                            else Right . trim . joinLines $ gen <$> tops
    sourceLines = lines source 
    ast = parseRules source

funcBlock ind (FuncDef name params body) = concat 
  [ indent ind
  , "function "
  , name
  , "(", intercalate ", " params, ") { return ", body', "; }"
  ]
  where
    body' = trim . unlines $ (indent (ind + 1) ++) <$> lines body

typeRefList :: Int -> [TypeRef] -> String
typeRefList ind refs = 
  trim .  intercalate " | " $ ref <$> refs
  where
    ref (TypeNameRef name) = name
    ref (InlineTypeRef def) = typeBlock (ind + 1) def

typeBlock :: Int -> TypeDef -> String
typeBlock ind (TypeDef fields) = block ind $
  f <$> fields
  where
    f (Field r name refs) = name ++ ": " ++ typeRefList ind refs


typeFunc :: String -> [TypeRef] -> String
typeFunc name refs = 
  concat [ "function is"
  , capitalize name, " (resource) {\n  return " 
  , intercalate "\n  || " $ refCheck Nothing "resource" <$> refs
  , ";"
  , "\n}"
  ]
  where
    isReq (Field r _ _) = r
    key (Field _ n _) = n
    req = filter isReq
    primitives :: [String]
    primitives = words "list string bool timestamp null int float"
    defCheck :: String -> TypeDef -> String
    defCheck parent (TypeDef fields) = concat $ (
      [ parent, ".keys().hasAll(['",  intercalate "', '" requiredKeys, "'])"
      , "\n  && "++parent++".size() >= " ++ show mn
      , "\n  && "++parent++".size() <= " ++ show mx
      ] ++ fmap (("\n  && "++) . fieldCheck parent) fields)
        where
          requiredKeys = fmap key . req $ fields
          mx = length fields
          mn = length . req $ fields
    refCheck parent name (TypeNameRef t) =
      if t `elem` primitives then prim else func
        where
          prim = _addr ++ " is " ++ t
          func = "is" ++ capitalize t ++ "(" ++ _addr ++ ")"
          _addr = addr parent name 
    refCheck parent name (InlineTypeRef def) = 
      defCheck (addr parent name) def

    fieldCheck parent (Field r n refs) = if r 
      then "(" ++ rs ++")"
      else "(!"++parent++".hasAny(['"++n++"'])\n  || ("++ rs ++"))"
        where
          rs = intercalate "\n  || " $ refCheck (Just parent) n <$> refs
      
    addr Nothing n = n
    addr (Just p) n = p ++ "." ++ n
    

gen (TopLevelFunc def) = funcBlock 0 def
gen (TopLevelType name refs) = --"type " ++ name ++ " = " ++ (typeRefList 0 refs)
  typeFunc name refs 
--typeFunc :: String -> TypeDef -> String
gen (TopLevelPath def) = pathBlock 0 def
  where
    pathBlock ind (PathDef parts refs bodyItems) =
      joinLines . filter (/="") $
        [ indent ind ++ "match /" ++ pathHead parts ++ " {"
        , pathTypeFunc ind refs
        , pathBody ind bodyItems
        , pathTypeDir ind refs
        , indent ind ++ "}"
        ]
    ifNo xs i e = if length xs == 0 then i else e
    shiftBy ind s = joinLines $ (indent ind++) <$> lines s
    pathTypeDir ind refs = ifNo refs "" $ indent (ind+1) ++ "allow write: if is__pathType(resource);"
    pathTypeFunc ind refs = ifNo refs "" . shiftBy (ind+1) $ typeFunc "__pathType" refs
    pathHead parts = intercalate "/" $ pathPart <$> parts
    pathBody ind bodyItems = joinLines $ pathBodyItem ind <$> bodyItems
    pathBodyItem ind (PathBodyDir (PathDirective ops cond)) =
      concat [indent ind, "allow ", intercalate ", " ops, ": if ", cond, ";"]
    pathBodyItem ind (PathBodyFunc def) =
      funcBlock (ind + 1) def
    pathBodyItem ind (PathBodyPath def) = 
      pathBlock (ind + 1) def

    pathPart (PathPartVar v) = concat ["{", v, "}"]
    pathPart (PathPartWild w) = concat ["{", w, "=**}"]
    pathPart (PathPartStatic s) = s
