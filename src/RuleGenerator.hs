module RuleGenerator (
  generate
) where

-- Definitions:
-- _type functions_ are functions that check to make sure that the data resource confirms to the type structure.
-- _type refs_ are references to types. E.g. in `{name: FullName}` FullName is a reference to a type.
-- _type defs_ are definitions of types. E.g. the whole `{name: FullName}` is a definition.
-- _inline type defs_ are defined inline. E.g. in `{name: {first: string, last: string}}` {first: string, last: string} is inline.

import Parser
import RuleParser
import RuleLang
import Error (Error(..))
import Loc (loc, Loc)
import Data.List (findIndices, intercalate, stripPrefix)
import Data.Char (toUpper)
import Data.Maybe (maybe)
import CodePrinter
  
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
    finalize :: [TopLevel] -> CodePrinter -> String
    finalize tops lines = printCode 0 $ do
      if wrap then do
        _print "service cloud.firestore {"
        _indent
        _return
        _print "match /databases/{database}/documents {"
        _indent
        _return
        optVars tops
        _return
        lines
        _deindent
        _return
        _print "}"
        _deindent
        _return
        _print "}"
        else lines

    optVar (TopLevelOpt name val) = _print $ name ++ " = " ++ val ++ ";"
    optVar _ = _print ""
    optVars tops = _lines $ optVar <$> tops 
    tree :: ParserResult [TopLevel]-- [([TopLevel], String)]
    tree = parseRules source
    q :: ParserResult [TopLevel] -> Either String String
    q (Right (tops, unparsed, l, c)) = 
      if length unparsed > 0
         then Left ("Could not parse on\n  on " ++ printLoc l c)
         else Right . finalize tops $ genTops tops -- gen <$> tops
    q (Left (Just (error, l, c))) = Left (error ++ "\n  on " ++ printLoc l c)
    q (Left Nothing) = Left ("Unexpected parser error.")
    genTops tops = _lines $ gen <$> tops


funcBlock (FuncDef name params body) = _function name params (_print body)

typeFuncName typeName = "is____" ++ capitalize typeName

data FuncParam = FuncParam (Maybe String) String
--
-- the main recursive function to generate the type function
typeFunc :: String -> [TypeRef] -> CodePrinter
typeFunc name refs = 
  let refCheckList = refCheck (FuncParam Nothing "data") (FuncParam Nothing "prev") False <$> refs 
  in _function (typeFuncName name) ["data", "prev"] (_linesWith _or refCheckList)
  where
    isReq (Field r _ _ _) = r 
    key (Field _ n _ _) = n
    onlyRequired = filter isReq
    addr :: FuncParam -> String
    addr (FuncParam Nothing n) = n
    addr (FuncParam (Just p) n) = p ++ "." ++ n

    defCheck :: String -> String -> TypeDef -> CodePrinter
    defCheck parent prevParent (TypeDef fields) = do --concat $
      keyPresenceCheck
      _return
      _and
      _linesWith _and fieldChecks
        where
          requiredKeys = fmap key . onlyRequired $ fields
          mx = length fields
          mn = length . onlyRequired $ fields
          fieldChecks = --intercalate (line0++"&& ") $ fmap (fieldCheck (ind + 2) parent prevParent) fields
            fieldCheck parent prevParent <$> fields
          keyPresenceCheck = do
            _linesWith _and . fmap _print . filter (\s->s/="") $
              [ if length requiredKeys > 0
                   then _hasAll parent requiredKeys
                   else ""
              , _sizeBetween parent mn mx
              , _hasOnly parent (fieldName <$> fields)
              ]


    refCheck :: FuncParam -> FuncParam -> Bool -> TypeRef -> CodePrinter
    refCheck curr prev _const (InlineTypeDef def) = 
      defCheck (addr curr) (addr prev) def
    refCheck pcurr@(FuncParam parent curr) pprev@(FuncParam prevParent prev) _const (TypeNameRef t arrSize) = 
      if t `elem` primitives then primType arrSize else func
      where
        listCond = _addr ++ " is list"

        primType :: (Maybe Int) -> CodePrinter -- primitive types
        primType Nothing  
                     | t=="null" = _print $ _addr ++ " == null "   
                     | t=="float" = _print $ "(" ++ _addr ++ " is float || " ++ _addr ++ " is int)"
                     | otherwise = _print $ _addr ++ " is " ++ t
        primType (Just n) = do
          _print listCond          
          _printIf (n > 0) $ do
            _return
            _and
            arrElemCheck n

        sizeCheck i = 
           "(" ++ _sizeLte _addr i ++ " || " ++ _addr ++ "[" ++ show (i-1) ++ "] is " ++ t ++ ")"
        arrElemCheck n = do
          _linesWith _and [ _print $ sizeCheck i | i <- [1..n] ]

        -- func is defined like this because firestore does not allow tertiary logic (?:) or similar.
        func = _print $ "(" ++ _prevParent ++ "==null && " ++ funcwp "null" ++ " || " ++ funcwp _prevParent ++ ")"
        funcwp p = typeFuncName t ++ "(" ++ _addr ++ ", " ++ p ++ ")"
        _addr = addr pcurr
        _prevAddr = addr pprev
        _prevParent = maybe "prev" id prevParent

    fieldCheck :: String -> String -> Field -> CodePrinter
    fieldCheck parent prevParent (Field r n refs c) = do
      _printIf c $ do
        _print constCheck
        _return
        _and
      _printIf r $ do
        formattedRefs
      _printIf (not r) $ do
        _print "("
        _indent
        _return
        _print $ notDefined parent n
        _return
        _or
        formattedRefs
        _deindent
        _return
        _print ")"
      where
        formattedRefs =
          if length refs == 1
             then rs
             else do
               _print "("
               _indent
               _return
               rs
               _deindent
               _line $ _print ")"

        notDefined parent key = "!" ++ _hasAny parent [key]
        rs = _linesWith _or (refCheck curr prev c <$> refs)
        curr = FuncParam (Just parent) n
        prev = FuncParam (Just prevParent) n
        constCheck = check -- const type check
          where check = "(" ++ prevParent ++ "==null || !" ++ _hasAll prevParent [n] ++ " || " ++ _prevAddr ++ "==null || " ++_addr++"=="++_prevAddr++")"
                _prevAddr = addr prev
                _addr = addr curr
      
    

gen :: TopLevel -> CodePrinter
gen (TopLevelOpt name val) = _print "" -- this will be generated after wrapping the code
gen (TopLevelFunc def) = funcBlock def
gen (TopLevelType name refs) = typeFunc name refs
gen (TopLevelPath def) = pathBlock def
  where
    pathBlock (PathDef parts refs bodyItems) = do
      _print "match /"
      _print $ pathHead parts
      _print " {"
      _indent
      _return
      _printIf (length refs > 0) $ do
        pathTypeFunc refs
        _return
      pathBody (augmentWithType bodyItems refs)
      _deindent
      _return
      _print "}"

    ifNo xs i e = if length xs == 0 then i else e
    pathTypeCond = "(resource==null && " 
                    ++ typeFuncName pathTypeName ++ "(request.resource.data, null) || " 
                    ++ typeFuncName pathTypeName ++ "(request.resource.data, resource.data))"
    pathTypeDir = PathBodyDir (PathDirective ["write"] pathTypeCond) 
    --
    -- do nothing if no refs provided
    -- augment each if bodyItems contains write update or create
    -- add a write otherwise
    --
    augmentWithType :: [PathBodyItem] -> [TypeRef] -> [PathBodyItem]
    augmentWithType bodyItems [] = bodyItems
    augmentWithType [] refs = [ pathTypeDir ]
    augmentWithType bodyItems refs = if hasWriteDirs bodyItems
      then withRefCheck <$> bodyItems 
      else bodyItems

    withRefCheck :: PathBodyItem -> PathBodyItem
    withRefCheck item = if hasWriteOps item 
      then insertRefCheck item
      else item

    insertRefCheck :: PathBodyItem -> PathBodyItem
    insertRefCheck (PathBodyDir (PathDirective ops cond)) =
      PathBodyDir $ PathDirective ops (pathTypeCond ++ " && (" ++ cond ++ ")")

    insertRefCheck x = x

    hasWriteDirs = not . null . writeDirs
    writeDirs bodyItems = filter hasWriteOps bodyItems 
    hasWriteOps :: PathBodyItem -> Bool
    hasWriteOps (PathBodyDir (PathDirective ops cond)) = 
      ops `hasAnyOf` ["write", "update", "create"]
    hasWriteOps _ = False

    pathTypeFunc :: [TypeRef] -> CodePrinter
    pathTypeFunc refs = --ifNo refs "" . shiftBy (ind + 2) $ typeFunc pathTypeFuncName refs
      _printIf (length refs > 0) $ typeFunc pathTypeName refs
    
    pathTypeName = "__PathType"

    pathHead parts = intercalate "/" $ pathPart <$> parts

    pathBody :: [PathBodyItem] -> CodePrinter
    pathBody bodyItems = do
      _lines $ pathBodyItem <$> bodyItems

    pathBodyItem :: PathBodyItem -> CodePrinter  
    pathBodyItem (PathBodyDir (PathDirective ops cond)) = 
      _print $ concat ["allow ", intercalate ", " ops, ": if ", cond, ";"]
    pathBodyItem (PathBodyFunc def) = funcBlock def
    pathBodyItem (PathBodyPath def) = pathBlock def

    pathPart :: PathPart -> String 
    pathPart (PathPartVar v) = concat ["{", v, "}"]
    pathPart (PathPartWild w) = concat ["{", w, "=**}"]
    pathPart (PathPartStatic s) = s
