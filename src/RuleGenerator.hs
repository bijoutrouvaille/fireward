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
primitives = words "list string bool timestamp null int float map any"

-- the main exported function. calls the `gen` function internally.
generate :: Bool -> String -> Either String String
generate wrap source = q tree
  where
    finalize :: [TopLevel] -> CodePrinter -> String
    finalize tops lines = printCode 0 $ do
      if wrap then do
        optVars tops
        _return
        _print "service cloud.firestore {"
        _indent
        _return
        _print "match /databases/{database}/documents {"
        _indent
        _return
        lines
        _deindent
        _return
        _print "}"
        _deindent
        _return
        _print "}"
        else lines

    isOptVar (TopLevelOpt _ _) = True
    isOptVar _ = False
    optVar (TopLevelOpt name val) = _print $ name ++ " = " ++ val ++ ";"
    optVar _ = _print ""
    optVars tops = _lines . fmap optVar . filter isOptVar $ tops 
    tree :: ParserResult [TopLevel]
    tree = parseRules source
    q :: ParserResult [TopLevel] -> Either String String
    q (Right (tops, unparsed, l, c)) = 
      if length unparsed > 0
         then Left ("Could not parse on\n  on " ++ printLoc l c)
         else Right . finalize tops $ genTops tops 
    q (Left (Just (error, l, c))) = Left (error ++ "\n  on " ++ printLoc l c)
    q (Left Nothing) = Left ("Unexpected parser error.")
    genTops tops = _lines $ gen <$> tops


funcBlock (FuncDef name params body) = _function name params (_print body)

typeFuncName typeName = "is____" ++ capitalize typeName


-- data NodeLoc = NodeLoc (Maybe String) (Either String Int)
data NodeLoc = NodeIndex NodeLoc Int | NodeProp (Maybe NodeLoc) String
addr :: NodeLoc -> String
addr (NodeIndex root i) = addr root ++ "[" ++ show i ++ "]"
addr (NodeProp Nothing prop) = prop
addr (NodeProp (Just parent) prop) = addr parent ++ "." ++ prop

exsts :: NodeLoc -> String
exsts (NodeIndex par i) = exsts par ++ " && " ++ addr par ++ " is list && " ++ addr par ++ ".size() > " ++ show i
exsts (NodeProp Nothing prop) = prop ++ "!=null"
exsts (NodeProp (Just parent) prop) = exsts parent ++ " && '" ++ prop ++ "' in " ++ addr parent 

--
-- the main recursive function to generate the type function
typeFunc :: String -> [TypeRef] -> CodePrinter
typeFunc name refs = 
  let refCheckList = refCheck (NodeProp Nothing "data") (NodeProp Nothing "prev") False <$> refs 
  in _function (typeFuncName name) ["data", "prev"] (_linesWith _or refCheckList)
  where
    isReq (Field r _ _ _) = r 
    key (Field _ n _ _) = n
    onlyRequired = filter isReq

    -- addr (NodeLoc Nothing (Left n)) = n
    -- addr (NodeLoc Nothing (Right i)) = fail "Unexpected node location; cannot address."-- n ++ "[" ++ show i ++ "]"
    -- addr (NodeLoc (Just p) (Left n)) = p ++ "." ++ n
    -- addr (NodeLoc (Just p) (Right i)) = p ++ "[" ++ show i ++ "]"

    -- exsts (NodeLoc Nothing (Left n)) = "true"
    -- exsts (NodeLoc Nothing (Right i)) = fail "Unexpected node location; cannot exist." -- n ++ " is list && " ++ n ++ ".size() >=" ++ show i 
    -- exsts (NodeLoc (Just p) (Left n)) = p ++ ".keys().hasAll['" ++ n ++ "']"
    -- exsts (NodeLoc (Just p) (Right i)) = p ++ " is list && " ++ p ++ ".size() > " ++ show i 

    nodeParent (NodeIndex p _) = Just p
    nodeParent (NodeProp p _) = p
    

    defCheck :: NodeLoc -> NodeLoc -> TypeDef -> CodePrinter
    defCheck parent prevParent (TypeDef fields validations) = do 
      _printIf (length validations > 0) $ do
        _print "("
        _indent
        _return
        _linesWith _and (fmap validation validations)
        _deindent
        _return
        _print ") "
        _and
      keyPresenceCheck
      _return
      _printIf (length fields > 0) $ do
        _and
        _linesWith _and fieldChecks
        where
          validation (ValidationExpr methods body) = 
            _linesWith _and (fmap (validationItem body) methods)
          validationItem body m = do
            _print "( " 
            _print ("request.method != '" ++ show m ++ "' || ")
            _print "( "
            _print body
            _print " )"
            _print " )"
            
          requiredKeys = fmap key . onlyRequired $ fields
          mx = length fields
          mn = length . onlyRequired $ fields
          fieldChecks = fieldCheck parent prevParent <$> fields
          keyPresenceCheck = do
            _linesWith _and . fmap _print . filter (\s->s/="") $
              [ if length requiredKeys > 0
                   then _hasAll (addr parent) requiredKeys
                   else ""
              , _sizeBetween (addr parent) mn mx
              , _hasOnly (addr parent) (fieldName <$> fields)
              ]


    refCheck :: NodeLoc -> NodeLoc -> Bool -> TypeRef -> CodePrinter
    refCheck curr prev _const (LiteralTypeRef val) =
      _print $ (addr curr) ++ " == " ++ val
    refCheck curr prev _const (InlineTypeDef def) = 
      defCheck curr prev def
    refCheck curr prev _const (ListTypeRef ref) =
      _print $ addr curr ++ " is list"
    refCheck curr prev _const (GroupedTypeRef refs) = do
      _print "("
      _indent; _return
      multiRefCheck curr prev refs
      _deindent; _return
    refCheck curr prev c (TupleTypeRef fs) = do
      _print "( "
      _print $ _addr ++ " is list " 
      _and
      _print $ _sizeLte _addr maxSize
      _print " "
      _and
      _print $ _sizeGte _addr minSize
      _indent
      _return
      _and
      _linesWith _and [ _refLine r | r <- refs ]
      _deindent
      _return 
      _print ")"
      where
        maxSize = length fs
        minSize = length . dropWhile ((==False) . fst) . reverse $ fs
        refs :: [(Bool, [TypeRef], Int)]
        refs = [ (req, ref, i) | i <- [0..length fs - 1], let (req, ref) = fs !! i ]
        _iaddr :: Int -> String
        _iaddr i = _addr ++ "[" ++ show i ++ "]"
        _addr = addr curr
        _prevAddr = addr prev
        _iloc i = NodeIndex curr i -- NodeLoc (Just _addr) (Right i)
        _refLine (req, refs, i) = do
          _print "("
          _indent
          _return
          _printIf req $ do
            _print $ exsts (_iloc i)
            _print " "
            _and
          _printIf (not req) $ do
            _print $ "!(" ++ exsts (_iloc i) ++ ") "
            _or
            _print " "
            _print $ _iaddr i ++ " == null "
            _print " "
            _or

          multiRefCheck (_iloc i) (NodeIndex (prev) i) refs
          _deindent
          _return
          _print ")"
          
      
    refCheck curr prev _const (TypeNameRef t) = 
      if t `elem` primitives then primType else func
      where
        primType :: CodePrinter -- primitive types
        primType 
          | t=="any" = _print "true"
          | t=="null" = _print $ _addr ++ " == null "   
          | t=="float" = _print $ "(" ++ _addr ++ " is float || " ++ _addr ++ " is int)"
          | otherwise = _print $ _addr ++ " is " ++ t

        -- func is defined like this because firestore does not allow tertiary logic (?:) or similar.
        -- !(p==null || !p.k) === p!=null && p.k | DeMorgan law
        func = do
          _print "("
          -- _printIf (prevParent /= Nothing) $ do
          --   _print "("
          --   _print $ _prevParent ++ "==null"
          --   _print " "
          --   _or
          --   _print $ "!(" ++ exsts prev ++ ")"
          --   _print ")"
          -- _printIf (prevParent == Nothing) $ do
          --   _print $ _prevParent ++ "==null"
          _print $ "!(" ++ exsts prev ++ ") "
          _and
          _print $ funcwp "null"
          _print " "
          _or
          -- _print $ _prevParent ++ "!=null "
          -- _and
          -- _printIf (prevParent /= Nothing) $ do
          --   _print $ exsts prev
          --   _print " "
          --   _and
          _print $ exsts prev
          _print " "
          _and
          _print $ funcwp _prevAddr
          _print ")"

        funcwp parent' = typeFuncName t ++ "(" ++ _addr ++ ", " ++ parent' ++ ")"
        _addr = addr curr
        _prevAddr = addr prev
        _prevParent = maybe "prev" id prevParent
        prevParent = addr <$> nodeParent prev



    multiRefCheck :: NodeLoc -> NodeLoc -> [TypeRef] -> CodePrinter
    multiRefCheck curr prev refs = do
      if length refs == 1
         then refLines
         else do
           _print "("
           _indent
           _return
           refLines     
           _deindent
           _line $ _print ")"
      where refLines = _linesWith _or (refCheck curr prev False <$> refs)


    fieldCheck :: NodeLoc -> NodeLoc -> Field -> CodePrinter
    fieldCheck parent prevParent (Field r n refs c) = do
      _printIf c $ do -- c means field is marked as const
        constCheck
        _return
        _and
      _printIf r $ do
        formattedRefs
      _printIf (not r) $ do
        _print "("
        _indent
        _return
        _print $ notDefined n
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

        notDefined key = "!" ++ _hasAny (addr parent) [key]
        rs = _linesWith _or (refCheck curr prev c <$> refs)
        curr = NodeProp (Just parent) n
        prev = NodeProp (Just prevParent) n
        _prevAddr = addr prev
        _addr = addr curr
        constCheck = do-- const type check
          _print "(" 
          _print $ addr prevParent ++ "==null " 
          _or
          _print $ "!" ++ _hasAll (addr prevParent) [n] ++ " "
          _or
          _print $ _prevAddr ++ "==null "
          _or
          _print $ _addr ++ "==" ++ _prevAddr
          _print ")"
      
    

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
    pathTypeCond = "(!(resource!=null && resource.data!=null) && " 
                    ++ typeFuncName pathTypeName ++ "(request.resource.data, null) || " 
                    ++ "resource!=null && resource.data!=null && "
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
