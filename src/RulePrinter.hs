module RuleGeneratorX (
  generate
) where

-- Definitions:
-- _type functions_ are functions that check to make sure that the data resource confirms to the type structure.
-- 
--

import Parser
import RuleParser
import RuleLang
import Error (Error(..))
import Loc (loc, Loc)
import Data.List (findIndices, intercalate, stripPrefix)
import Data.Char (toUpper)
import Data.Maybe (maybe)
-- import LogicPrinter  (Expr(..), Op(..), printLogic)
  
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
printLoc l c = "line " ++ show (l+1) ++", column "++show (c+1)

primitives :: [String]
primitives = words "list string bool timestamp null int float map"

-- the main exported function. calls the `gen` function internally.
generate :: Bool -> String -> Either String String
generate wrap source = q tree
  where
    finalize :: [TopLevel] -> [String] -> String
    finalize tops lines = joinLines $ if wrap then optVars tops ++ withBoilderplate (unlines lines) else lines
    optVar (TopLevelOpt name val) = name ++ " = " ++ val ++ ";"
    optVar _ = ""
    optVars tops = optVar <$> tops 
    tree :: ParserResult [TopLevel]-- [([TopLevel], String)]
    tree = parseRules source
    q :: ParserResult [TopLevel] -> Either String String
    q (Right (tops, unparsed, l, c)) = 
      if length unparsed > 0
         then Left ("Could not parse on\n  on " ++ printLoc l c)
         else Right . finalize tops $ gen <$> tops
    q (Left (Just (error, l, c))) = Left (error ++ "\n  on " ++ printLoc l c)
    q (Left Nothing) = Left ("Unexpected parser error.")

funcBlock ind (FuncDef name params body) = concat 
  [ indent ind
  , "function "
  , name
  , "(", intercalate ", " params, ") { return ", body', "; }"
  ]
  where
    body' = trim . unlines $ (indent (ind + 2) ++) <$> lines body


withBoilderplate :: String -> [String]
withBoilderplate code = 
  [ "service cloud.firestore {"
  , "  match /databases/{database}/documents {"
  , ""
  ]
  ++ indented ++
  [ ""
  , "  }"
  , "}"
  ]
  where 
    r = lines code
    indented = indentLinesBy2 r :: [String]
    indentLinesBy2 = fmap (shift 2 ++)
    shift n = take (n*2) $ repeat ' '

typeFuncName typeName = "is____" ++ typeName
typeFunc2 name refs = 
  _function (typeFuncName name) ["data", "prev"] (_linesWith "|| " $ refTests)
  where
    refTests = testRef (FuncParam Nothing "data") (FuncParam Nothing "prev") False <$> refs
    refTest ref = 
    refTest :: FuncParam -> FuncParam -> Bool -> TypeRef -> CodePrinter
    refTest ind curr prev _const (InlineTypeDef def) = 
      defCheck ind (addr curr) (addr prev) def
    refTest ind pcurr@(FuncParam parent curr) pprev@(FuncParam prevParent prev) _const (TypeNameRef t arrSize) = 

data FuncParam = FuncParam (Maybe String) String
-- the main recursive function to generate the type function
typeFunc :: String -> [TypeRef] -> String
typeFunc name refs = 
  concat [ "function "
  , funcName name, "(data, prev) {\n  return " 
  , intercalate "\n  || " $ refCheck 0 (FuncParam Nothing "data") (FuncParam Nothing "prev") False <$> refs
  , ";"
  , "\n}"
  ]
  where
    isReq (Field r _ _ _) = r 
    key (Field _ n _ _) = n
    req = filter isReq
    addr :: FuncParam -> String
    addr (FuncParam Nothing n) = n
    addr (FuncParam (Just p) n) = p ++ "." ++ n
    funcName name = "is" ++ capitalize name

    defCheck :: Int -> String -> String -> TypeDef -> String
    defCheck ind parent prevParent (TypeDef fields) = --concat $
      keyPresenceCheck ++ fieldChecks--(fmap ((line0 ++ " && ") ++) fieldChecks)
        where
          initial = if ind==2 then "  " else " "
          line0 = ("\n" ++ indent (ind + 2))
          line = ("\n" ++ indent (ind + 4))
          requiredKeys = fmap key . req $ fields
          mx = length fields
          mn = length . req $ fields
          fieldChecks = intercalate (line0++"&& ") $ fmap (fieldCheck (ind + 2) parent prevParent) fields
          keyPresenceCheck = concat [ ""
                     , if length requiredKeys > 0 
                          then _hasAll parent requiredKeys-- parent++".keys().hasAll(['" ++  intercalate "', '" requiredKeys ++ "'])" ++ line ++ "&& " 
                          else ""
                     , line, "&& ",_sizeBetween parent mn mx --parent++".size() >= " ++ show mn
                     -- , line, "&& ", _sizeLtw parent mx --parent++".size() <= " ++ show mx
                     , line, "&& ", _hasOnly parent (fieldName <$> fields)-- parent ++ ".keys().hasOnly(['" ++ intercalate "', '" (fmap fieldName fields) ++ "'])"
                     ]

    refCheck :: Int -> FuncParam -> FuncParam -> Bool -> TypeRef -> String
    refCheck ind curr prev _const (InlineTypeDef def) = 
      defCheck ind (addr curr) (addr prev) def
    refCheck ind pcurr@(FuncParam parent curr) pprev@(FuncParam prevParent prev) _const (TypeNameRef t arrSize) = 
      if t `elem` primitives then prim arrSize else func
      where
        listCond = _addr ++ " is list"
        prim :: (Maybe Int) -> String
        prim size = primType size
        -- eq = if _const then q else ""
        --   where q = "(" ++ p ++ "==null || !" ++ p ++ ".keys().hasAll(['" ++ curr ++ "']) || " ++ _prevAddr ++ "==null || " ++_addr++"=="++_prevAddr++")\n" ++ indent (ind + 2) ++ "&& "
        --         p = maybe "prev" id prevParent
        primType :: (Maybe Int) -> String -- primitive types
        primType Nothing  
                     | t=="null" = _addr ++ " == null "   
                     | t=="float" = "(" ++ _addr ++ " is float || " ++ _addr ++ " is int)"
                     | otherwise = _addr ++ " is " ++ t
        primType (Just n) = listCond ++ (if n == 0 then "" else "\n" ++ indent (ind+1) ++  " && "  ++ arrElemCheck n)
        sizeCheck i = 
           "(" ++ _sizeLte _addr i ++ " || " ++ _addr ++ "[" ++ show (i-1) ++ "] is " ++ t ++ ")"
        arrElemCheck n = intercalate ("\n" ++ indent (ind+1) ++ " && ") [ sizeCheck i | i <- [1..n] ]
        -- func is defined like this because firestore does not allow tertiary logic (?:) or similar.
        func = "(" ++ _prevParent ++ "==null && " ++ funcwp "null" ++ " || " ++ funcwp _prevParent ++ ")"
        funcwp p = funcName t ++ "(" ++ _addr ++ ", " ++ p ++ ")"
        _addr = addr pcurr
        _prevAddr = addr pprev
        _prevParent = maybe "prev" id prevParent

    fieldCheck :: Int -> String -> String -> Field -> String
    fieldCheck ind parent prevParent (Field r n refs c) = 
      if r
      then constCheck ++ formattedRefs
      else line0 ++ constCheck ++ "!" ++ parent ++ ".keys().hasAny(['" ++ n ++ "'])" ++ line ++ "|| " ++ formattedRefs ++ line0
        where
          formattedRefs = 
            if length refs == 1
            then rs
            else "(" ++ line ++ "   " ++ rs ++ line ++")"
          rs = intercalate (line ++"|| ") $ refCheck ind curr prev c <$> refs
          linei = "\n" ++ indent (ind)
          line0 = "\n" ++ indent (ind + 2)
          line = "\n" ++ indent (ind + 4)
          curr = FuncParam (Just parent) n
          prev = FuncParam (Just prevParent) n
          constCheck = if c then check else "" -- const type check
            where check = "&& (" 
                              ++ parent ++ "==null || !" 
                              ++ parent ++ ".keys().hasAll(['" ++ n ++ "']) || " 
                              ++ _prevAddr ++ "==null || " 
                              ++ _addr ++ "==" 
                              ++ _prevAddr ++ ")\n" ++ indent (ind + 2) ++ "&& "
                  _prevAddr = addr prev
                  _addr = addr curr
      
    

gen :: TopLevel -> String
gen (TopLevelOpt name val) = "" -- this will be generated after wrapping the code
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
    pathTypeCond = "is" ++pathTypeFuncName ++ "(request.resource.data, resource==null ? null : resource.data)"
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

