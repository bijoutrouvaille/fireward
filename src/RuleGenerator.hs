module RuleGenerator (
  Error(..),
  Loc(..),
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

xs `hasAnyOf` ys = not . null $ filter (flip elem $ ys) xs

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
    body' = trim . unlines $ (indent (ind + 2) ++) <$> lines body

typeRefList :: Int -> [TypeRef] -> String
typeRefList ind refs = 
  trim .  intercalate " | " $ ref <$> refs
  where
    ref (TypeNameRef name) = name
    ref (InlineTypeRef def) = typeBlock (ind + 2) def

typeBlock :: Int -> TypeDef -> String
typeBlock ind (TypeDef fields) = block ind $
  f <$> fields
  where
    f (Field r name refs) = name ++ ": " ++ typeRefList ind refs


typeFunc :: String -> [TypeRef] -> String
typeFunc name refs = 
  concat [ "function is"
  , capitalize name, " (resource) {\n  return " 
  , intercalate "\n  || " $ refCheck 0 Nothing "resource" <$> refs
  , ";"
  , "\n}"
  ]
  where
    isReq (Field r _ _) = r
    key (Field _ n _) = n
    req = filter isReq
    primitives :: [String]
    primitives = words "list string bool timestamp null int float"
    defCheck :: Int -> String -> TypeDef -> String
    defCheck ind parent (TypeDef fields) = concat $ (
      [ "("
      , line, parent++".keys().hasAll(['",  intercalate "', '" requiredKeys, "'])"
      , line, "&& ", parent++".size() >= " ++ show mn
      , line, "&& ", parent++".size() <= " ++ show mx
      ] ++ fmap ((line0++) . fieldCheck (ind + 2) parent) fields)
      ++ [line0, ")"]
        where
          initial = if ind==2 then "  " else " "
          line0 = ("\n" ++ indent (ind + 2))
          line = ("\n" ++ indent (ind + 4))
          requiredKeys = fmap key . req $ fields
          mx = length fields
          mn = length . req $ fields
    refCheck ind parent name (TypeNameRef t) =
      cond 
        where
          cond = if t `elem` primitives then prim else func
          prim = _addr ++ " is " ++ t
          func = "is" ++ capitalize t ++ "(" ++ _addr ++ ")"
          _addr = addr parent name 
    refCheck ind parent name (InlineTypeRef def) = 
      defCheck (ind) (addr parent name) def

    fieldCheck :: Int -> String -> Field -> String
    fieldCheck ind parent (Field r n refs) = if r 
      then "  && " ++ formattedRefs
      else "  && ("++line++"!"++parent++".hasAny(['"++n++"'])"++line++"|| "++ formattedRefs ++ line0 ++ ")"
        where
          formattedRefs = 
            if length refs == 1
            then rs
            else "(" ++ line ++ "   " ++ rs ++ line ++")"
          rs = intercalate (line ++"|| ") $ refCheck (ind) (Just parent) n <$> refs
          linei = "\n" ++ indent (ind)
          line0 = "\n" ++ indent (ind + 2)
          line = "\n" ++ indent (ind + 4)
      
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
        , pathBody ind (augmentWithType bodyItems refs)
        , indent ind ++ "}"
        ]
    ifNo xs i e = if length xs == 0 then i else e
    shiftBy ind s = joinLines $ (indent ind++) <$> lines s
    -- do nothing if no refs provided
    -- augment each if bodyItems contains write update or create
    -- add a write otherwise
    pathTypeCond = "is__pathType(request.resource.data)"
    pathTypeDir = PathBodyDir (PathDirective ["write"] pathTypeCond) 
    augmentWithType bodyItems [] = bodyItems
    augmentWithType [] refs = [ pathTypeDir ]
    augmentWithType bodyItems refs = if hasWriteDirs bodyItems
      then withRefCheck <$> bodyItems 
      else bodyItems
    withRefCheck item = if hasWriteOps item 
      then insertRefCheck item
      else item
    insertRefCheck (PathBodyDir (PathDirective ops cond)) =
      PathBodyDir $ PathDirective ops (pathTypeCond ++ " && (" ++ cond ++ ")")
    insertRefCheck x = x
    hasWriteDirs = not . null . writeDirs
    writeDirs bodyItems = filter hasWriteOps bodyItems 
    hasWriteOps (PathBodyDir (PathDirective ops cond)) = 
      ops `hasAnyOf` ["write", "update", "create"]
    hasWriteOps _ = False
    pathTypeFunc ind refs = ifNo refs "" . shiftBy (ind + 2) $ typeFunc "__pathType" refs
    pathHead parts = intercalate "/" $ pathPart <$> parts
    pathBody ind bodyItems = joinLines $ pathBodyItem ind <$> bodyItems
    pathBodyItem ind (PathBodyDir (PathDirective ops cond)) = 
      
      concat [indent (ind + 2), "allow ", intercalate ", " ops, ": if ", cond, ";"]
    pathBodyItem ind (PathBodyFunc def) =
      funcBlock (ind + 2) def
    pathBodyItem ind (PathBodyPath def) = 
      pathBlock (ind + 2) def

    pathPart (PathPartVar v) = concat ["{", v, "}"]
    pathPart (PathPartWild w) = concat ["{", w, "=**}"]
    pathPart (PathPartStatic s) = s
