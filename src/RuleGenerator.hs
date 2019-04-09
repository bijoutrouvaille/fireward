module RuleGenerator (
  generate
) where

-- Definitions:
-- _type functions_ are functions that check to make sure that the data resource confirms to the type structure.
-- 
--

import Parser
import RuleParser
import Error (Error(..))
import Loc (loc, Loc)
import Data.List (findIndices, intercalate, stripPrefix)
import Data.Char (toUpper)
import Data.Maybe (maybe)

  
getOr l n f = if length l < n 
                 then Right (l!!n) 
                 else Left f

xs `hasAnyOf` ys = not . null $ filter (flip elem $ ys) xs
xs `hasSameElems` ys = (xs `hasOnly` ys)  && length xs == length ys
xs `hasOnly` ys = null $ filter (flip elem $ ys) xs

capitalize "" = ""
capitalize (c:cs) = (toUpper c) : cs

joinLines = intercalate "\n"

indent n = take n $ repeat ' '
indentBy n = (indent n ++)


surround :: String -> String -> String -> String
surround b e s = concat [b,s,e]

-- the main exported function. calls the `gen` function internally.
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
                            then Left $ Error (loc source unparsed) "Could not parse."
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



-- the main recursive function to generate the type function
typeFunc :: String -> [TypeRef] -> String
typeFunc name refs = 
  concat [ "function "
  , funcName name, "(data) {\n  return " 
  , intercalate "\n  || " $ refCheck 0 Nothing "data" <$> refs
  , ";"
  , "\n}"
  ]
  where
    isReq (Field r _ _) = r 
    key (Field _ n _) = n
    req = filter isReq
    primitives :: [String]
    primitives = words "list string bool timestamp null int float"
    addr :: Maybe String -> String -> String
    addr Nothing n = n
    addr (Just p) n = p ++ "." ++ n
    funcName name = "is" ++ capitalize name
    defCheck :: Int -> String -> TypeDef -> String
    defCheck ind parent (TypeDef fields) = concat $
      keyCheck ++ fmap ((line0++) . fieldCheck (ind + 2) parent) fields
        where
          initial = if ind==2 then "  " else " "
          line0 = ("\n" ++ indent (ind + 2))
          line = ("\n" ++ indent (ind + 4))
          requiredKeys = fmap key . req $ fields
          mx = length fields
          mn = length . req $ fields
          keyCheck = [ ""
                     , parent++".keys().hasAll(['",  intercalate "', '" requiredKeys, "'])"
                     , line, "&& ", parent++".size() >= " ++ show mn
                     , line, "&& ", parent++".size() <= " ++ show mx
                     ]
    refCheck ind parent name (TypeNameRef t arrSize) = cond 
      where
        listCond = _addr ++ " is list"
        cond = if t `elem` primitives then prim arrSize else func
        optCond = "!" ++ _addr ++ ".keys().hasAny(['" ++ name ++ "']) || " ++ cond
        prim :: (Maybe Int)->String
        prim Nothing  
                     | t=="null" = _addr ++ " == null "   
                     | t=="float" = "(" ++ _addr ++ " is float || " ++ _addr ++ " is int)"
                     | otherwise = _addr ++ " is " ++ t
        prim (Just n) = listCond ++ (if n == 0 then "" else "\n" ++ indent (ind+1) ++  " && "  ++ arrElemCheck n)
        sizeCheck i = 
           "(" ++ _addr ++ ".size() <= " ++ show i ++ " || " ++ _addr ++ "[" ++ show (i-1) ++ "] is " ++ t ++ ")"
        arrElemCheck n = intercalate ("\n" ++ indent (ind+1) ++ " && ") [ sizeCheck i | i <- [1..n] ]
        func = funcName t ++ "(" ++ _addr ++ ")"
        _addr = addr parent name 
    refCheck ind parent name (InlineTypeRef def) = defCheck ind (addr parent name) def

    fieldCheck :: Int -> String -> Field -> String
    fieldCheck ind parent (Field r n refs) = if r
      then "  && " ++ formattedRefs
      else "  && ("++line++"!"++parent++".keys().hasAny(['"++n++"'])"++line++"|| "++ formattedRefs ++ line0 ++ ")"
        where
          formattedRefs = 
            if length refs == 1
            then rs
            else "(" ++ line ++ "   " ++ rs ++ line ++")"
          rs = intercalate (line ++"|| ") $ refCheck (ind) (Just parent) n <$> refs
          linei = "\n" ++ indent (ind)
          line0 = "\n" ++ indent (ind + 2)
          line = "\n" ++ indent (ind + 4)
      
    

gen (TopLevelFunc def) = funcBlock 0 def
gen (TopLevelType name refs) = typeFunc name refs
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
    pathTypeCond = "is" ++ pathTypeFuncName ++ "(request.resource.data)" 
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
    pathTypeFunc ind refs = ifNo refs "" . shiftBy (ind + 2) $ typeFunc pathTypeFuncName refs
    pathTypeFuncName = "__PathType"
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
