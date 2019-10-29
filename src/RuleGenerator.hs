module RuleGenerator (
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
import CodePrinter
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
    finalize :: [TopLevel] -> CodePrinter -> String
    finalize tops lines = printCode 0 $ do
      --if wrap then optVars tops ++ withBoilderplate (unlines lines) else lines
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
-- funcBlock ind (FuncDef name params body) = concat 
--   [ indent ind
--   , "function "
--   , name
--   , "(", intercalate ", " params, ") { return ", body', "; }"
--   ]
--   where
--     body' = trim . unlines $ (indent (ind + 2) ++) <$> lines body


-- withBoilderplate :: String -> [String]
-- withBoilderplate code = 
--   [ "service cloud.firestore {"
--   , "  match /databases/{database}/documents {"
--   , ""
--   ]
--   ++ indented ++
--   [ ""
--   , "  }"
--   , "}"
--   ]
--   where 
--     r = lines code
--     indented = indentLinesBy2 r :: [String]
--     indentLinesBy2 = fmap (shift 2 ++)
--     shift n = take (n*2) $ repeat ' '

typeFuncName typeName = "is____" ++ capitalize typeName

data FuncParam = FuncParam (Maybe String) String
-- the main recursive function to generate the type function
typeFunc :: String -> [TypeRef] -> CodePrinter
typeFunc name refs = 
  let refCheckList = refCheck (FuncParam Nothing "data") (FuncParam Nothing "prev") False <$> refs 
  in _function (typeFuncName name) ["data", "prev"] (_linesWith _or refCheckList)
  -- concat [ "function "
  -- , funcName name, "(data, prev) {\n  return " 
  -- , intercalate "\n  || " $ refCheck 0 (FuncParam Nothing "data") (FuncParam Nothing "prev") False <$> refs
  -- , ";"
  -- , "\n}"
  -- ]
  where
    isReq (Field r _ _ _) = r 
    key (Field _ n _ _) = n
    onlyRequired = filter isReq
    addr :: FuncParam -> String
    addr (FuncParam Nothing n) = n
    addr (FuncParam (Just p) n) = p ++ "." ++ n
    -- funcName name = "is" ++ capitalize name

    defCheck :: String -> String -> TypeDef -> CodePrinter
    defCheck parent prevParent (TypeDef fields) = do --concat $
      keyPresenceCheck
      _return
      _and
      -- _print _and
      -- _return
      _linesWith _and fieldChecks
      
      -- keyPresenceCheck ++ fieldChecks--(fmap ((line0 ++ " && ") ++) fieldChecks)
        where
          -- initial = if ind==2 then "  " else " "
          -- line0 = ("\n" ++ indent (ind + 2))
          -- line = ("\n" ++ indent (ind + 4))
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

                    -- concat [ ""
                    --  , if length requiredKeys > 0 
                    --       then _hasAll parent requiredKeys-- parent++".keys().hasAll(['" ++  intercalate "', '" requiredKeys ++ "'])" ++ line ++ "&& " 
                    --       else ""
                    --  , line, "&& ",_sizeBetween parent mn mx --parent++".size() >= " ++ show mn
                    --  -- , line, "&& ", _sizeLtw parent mx --parent++".size() <= " ++ show mx
                    --  , line, "&& ", _hasOnly parent (fieldName <$> fields)-- parent ++ ".keys().hasOnly(['" ++ intercalate "', '" (fmap fieldName fields) ++ "'])"
                    --  ]

    refCheck :: FuncParam -> FuncParam -> Bool -> TypeRef -> CodePrinter
    refCheck curr prev _const (InlineTypeDef def) = 
      defCheck (addr curr) (addr prev) def
    refCheck pcurr@(FuncParam parent curr) pprev@(FuncParam prevParent prev) _const (TypeNameRef t arrSize) = 
      if t `elem` primitives then primType arrSize else func
      where
        listCond = _addr ++ " is list"
        -- prim :: (Maybe Int) -> CodePrinter
        -- prim size = _print $ primType size
        -- eq = if _const then q else ""
        --   where q = "(" ++ p ++ "==null || !" ++ p ++ ".keys().hasAll(['" ++ curr ++ "']) || " ++ _prevAddr ++ "==null || " ++_addr++"=="++_prevAddr++")\n" ++ indent (ind + 2) ++ "&& "
        --         p = maybe "prev" id prevParent
        primType :: (Maybe Int) -> CodePrinter -- primitive types
        primType Nothing  
                     | t=="null" = _print $ _addr ++ " == null "   
                     | t=="float" = _print $ "(" ++ _addr ++ " is float || " ++ _addr ++ " is int)"
                     | otherwise = _print $ _addr ++ " is " ++ t
        primType (Just n) = do--listCond ++ (if n == 0 then "" else "\n" ++ indent (ind+1) ++  " && "  ++ arrElemCheck n)
          _print listCond          
          _printIf (n > 0) $ do
            -- _indent
            _return
            _and
            arrElemCheck n
            -- _deindent
            -- _return

        sizeCheck i = 
           "(" ++ _sizeLte _addr i ++ " || " ++ _addr ++ "[" ++ show (i-1) ++ "] is " ++ t ++ ")"
        arrElemCheck n = do
          --intercalate ("\n" ++ indent (ind+1) ++ " && ") [ sizeCheck i | i <- [1..n] ]
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
      -- _return
      -- if r
      -- then constCheck ++ formattedRefs
      -- else line0 ++ constCheck ++ notDefined parent key ++ line ++ "|| " ++ formattedRefs ++ line0
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

          -- if length refs == 1
          -- then rs
          -- else "(" ++ line ++ "   " ++ rs ++ line ++")"
        notDefined parent key = "!" ++ _hasAny parent [key]
        rs = _linesWith _or (refCheck curr prev c <$> refs)
        -- rs = intercalate (line ++"|| ") $ refCheck ind curr prev c <$> refs
        -- linei = "\n" ++ indent (ind)
        -- line0 = "\n" ++ indent (ind + 2)
        -- line = "\n" ++ indent (ind + 4)
        curr = FuncParam (Just parent) n
        prev = FuncParam (Just prevParent) n
        constCheck = check -- if c then check else "" -- const type check
          -- where check = "&& (" ++ parent ++ "==null || !" ++ parent ++ ".keys().hasAll(['" ++ n ++ "']) || " ++ _prevAddr ++ "==null || " ++_addr++"=="++_prevAddr++")\n" ++ indent (ind + 2) ++ "&& "
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

      -- _pathBlock (pathHead parts) (pathTypeFunc refs) (pathBody) 
      -- joinLines . filter (/="") $
      --   [ indent ind ++ "match /" ++ pathHead parts ++ " {"
      --   , pathTypeFunc ind refs
      --   , pathBody ind (augmentWithType bodyItems refs)
      --   , indent ind ++ "}"
      --   ]
    ifNo xs i e = if length xs == 0 then i else e
    -- shiftBy ind s = joinLines $ (indent ind++) <$> lines s
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
