module RuleParser 
( parseRules
, apply
, _typeDef
, _field
, _path
, _pathParts
, _pathVar
, _pathWild
, _pathStatic
, _pathDir
, _funcDef
-- , _funcBody
, _string
, _topLevelOptVar
, _literalTypeRef
, escape
, FuncDef (..)
, TypeDef (..)
, PathDef (..)
, PathPart (..)
, PathDirective (..)
, PathBodyItem (..)
, TypeRef (..)
, TopLevel (..)
, Field (..)
, RequestMethod (..)
, ValidationExpr (..)
, requestMethods
, writeRequestMethods
) where

import Parser
import Control.Applicative (optional)
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Combinators
import qualified ExprParser (expr) 
import qualified ExprPrinter (printExpr) 

data TopLevel = TopLevelType String [TypeRef]
              | TopLevelPath PathDef
              | TopLevelFunc FuncDef
              | TopLevelOpt String String
  deriving (Show, Eq)

type PathOp = String
type PathCondition = String
data PathDirective = PathDirective [PathOp] PathCondition
  deriving (Show, Eq)
data PathDef = PathDef [PathPart] [TypeRef] [PathBodyItem]
  deriving (Show, Eq)
data PathPart = PathPartStatic String 
              | PathPartVar String 
              | PathPartWild String
  deriving (Show, Eq)

data PathBodyItem = PathBodyDir PathDirective
                  | PathBodyPath PathDef
                  | PathBodyFunc FuncDef
  deriving (Show, Eq)

data FuncDef = FuncDef 
             { funcDefName :: String
             , funcDefParams :: [String] 
             , funcDefVars :: [(String, String)]
             , funcDefBody :: String
             } deriving (Show, Eq)

data RequestMethod = GET
                   | LIST
                   | CREATE
                   | UPDATE
                   | DELETE
                   deriving (Eq)
instance Show RequestMethod where
  show GET = "get"
  show LIST = "list"
  show CREATE = "create"
  show UPDATE = "update"
  show DELETE = "delete"

requestMethods =
  [ ("get", GET)
  , ("list", LIST)
  , ("create", CREATE)
  , ("update", UPDATE)
  , ("delete", DELETE)
  ]
writeRequestMethodList = [CREATE, UPDATE, DELETE]
writeRequestMethods = let prim = filter (\m -> snd m `elem` writeRequestMethodList) requestMethods
                          combined = [("write", writeRequestMethodList)]
                          in concat [fmap (\(s,m)->(s, [m])) prim, combined]




data ValidationExpr = ValidationExpr [RequestMethod] String
                    deriving (Eq, Show)
data TypeDef = TypeDef 
             { typeDefMembers :: [Field]
             , typeDefValidations :: [ValidationExpr]
             } deriving (Show, Eq)

-- TypeNameRef name-of-the-type (Maybe array-size) -- Nothing if not array or not array not finite
data TypeRef = TypeNameRef String
             | InlineTypeDef TypeDef 
             | LiteralTypeRef String
             | TupleTypeRef [(Bool, [TypeRef])]
             | ListTypeRef TypeRef
             | GroupedTypeRef [TypeRef]
             deriving (Show, Eq)
data Field = Field
           { required :: Bool
           , fieldName :: String
           , typeRefs :: [TypeRef]
           , constant :: Bool
           } deriving (Show, Eq)


_topLevelOptVar = do
  name <- token _var
  symbol "="
  optional space
  val <- require "configuration option value is not provided" $ _string <|> some digit
  optional space
  optional $ symbol ";"
  return $ TopLevelOpt name val
  where _var = _concat [ some $ _alpha <|> charIn "_", many $ _alphaNum <|> charIn "_" ] ""

_expr :: Parser String
_expr = do
  e <- ExprParser.expr
  return $ ExprPrinter.printExpr e

_funcVarDef :: Parser (String, String)
_funcVarDef = do
  symbol "let"
  name <- require "Variable name is required but missing." $ 
    token _expr
  require "Equal sign missing in variable definition." $ symbol "="
  val <- require "Variable value is missing." $
    token _expr
  return (name, val)

_funcDef :: Parser FuncDef
_funcDef = do
  symbol "function"
  name <- require "missing function name" $ token _varName
  params <- require ("function `"++name++"` is missing the parameter list") $ 
    grouped "(" ")" paramList
  require ("function `"++name++"` is missing an opening `{`") $ 
    symbol "{"
  vars <- many _funcVarDef
  optional $ symbol "return"
  body' <- optional $ token _expr 
  let body = maybe "" id body'

  optional $ symbol ";"

  require ("function `"++name++"` is missing a closing `}`") $ 
    symbol "}"

  guardWith ("function `"++name++"` is missing a body") (length body > 0)

  return $ FuncDef name params vars (trim body)

  where paramList = separated "," (token _varName)

_typeDefValidationExpr :: Parser ValidationExpr
_typeDefValidationExpr = do
  symbol "allow"
  optional space
  inputMethods <- manywith (symbol ",") $ token _varName
  let badMethods = (filter (\m -> Nothing == lookup m writeRequestMethods) inputMethods)
  guardWith ( "Invalid validation methods found: " ++ show badMethods)
    (length badMethods == 0) 

  let onlyJust = reverse . foldl (\a mv->maybe a (:a) mv) []
  let elemFrom d e = lookup e d
  let methods = concat . onlyJust . fmap (elemFrom writeRequestMethods) $ inputMethods
  guardWith "Validation expression must contain at least one request method (create, update, delete)" 
    (length methods > 0) 
  require "Validation expression missing a ':'" $ symbol ":"
  optional $ symbol "if"
  body <- require "Validation expressiom missing body" $ token _expr

  return $ ValidationExpr methods body
  


_typeDef :: Parser TypeDef
_typeDef = grouped "{" "}" $ do
  members <- manywith (optional $ symbol ",") _field
  optional $ symbol ","
  validations <- manywith (optional $ symbol ",") _typeDefValidationExpr
  return $ TypeDef members validations
  
_tupleTypeRef :: Parser TypeRef
_tupleTypeRef = do
  symbol "["
  refs <- manywith (symbol ",") _ref
  guardWith "Tuples must contain at least one item." (length refs > 0) 
  require "Missing a closing ']' for a tuple." $ symbol "]"
  return $ TupleTypeRef refs
  where _ref = do
          rs <- token _typeRefUnion
          q <- optional $ symbol "?"
          return (q == Nothing, rs)

_inlineTypeDefRef :: Parser TypeRef
_inlineTypeDefRef = InlineTypeDef <$> _typeDef
_atomTypeRef :: Parser TypeRef 
_atomTypeRef = _literalTypeRef <|> _singleTypeName <|> _inlineTypeDefRef <|> _tupleTypeRef

-- Extract all the type names into a flat array.
-- Useful for validation.
getNamed :: TypeRef -> [String]
getNamed (TypeNameRef name) = [name]
getNamed (ListTypeRef ref) = getNamed ref
getNamed (GroupedTypeRef refs) = foldMap getNamed refs
getNamed (TupleTypeRef tuple) = foldMap getNamed (foldMap snd tuple)
getNamed (InlineTypeDef _) = []
getNamed (LiteralTypeRef _) = []

_groupedTypeRef :: Parser TypeRef
_groupedTypeRef = do
  symbol "("
  refs <- manywith (symbol "|") $ _anyTypeRef
  guardWith "Parentheses must contain at least one type." (length refs > 0)
  require "A grouped type is missing the closing paren ')'" 
    $ symbol ")"
  return $ GroupedTypeRef refs

_listTypeRef :: Parser TypeRef
_listTypeRef = do
  ref <- _groupedTypeRef <|> _atomTypeRef
  symbol "["
  arrSize <- optional $ token _natural
  guardWith "Explicit list size must be between 1 and 12" (sizeCheck arrSize) 
  require "List is missing a closing paren." $ symbol "]"
  return $ res ref arrSize
  where
    res ref Nothing = ListTypeRef ref
    res ref (Just n) = TupleTypeRef [ (False, [ref]) | i <- [0..n-1]]
    sizeCheck Nothing = True
    sizeCheck (Just n) = n > 0 && n <= 12


_anyTypeRef :: Parser TypeRef
_anyTypeRef = do
  ref <- _listTypeRef <|> _groupedTypeRef <|> _atomTypeRef
  let names = getNamed ref
  let noNumber = all (/="number") names
  guardWith "type 'number' is disallowed to avoid a common source of confusion" noNumber
  return ref

_typeRefUnion :: Parser [TypeRef]
_typeRefUnion = manywith (symbol "|") $ token _anyTypeRef

_singleTypeName = do
  name <- token _varName
  return $ TypeNameRef name

-- _listTypeRef = do
  
  -- return . maybe (TypeNameRef name Nothing) $ \n ->
  --   TupleTypeRef [ (False, TypeNameRef name Nothing) ]
  -- return $ TypeNameRef name arrSize
_literalTypeRef :: Parser TypeRef
_literalTypeRef =
  LiteralTypeRef <$> token (tokenVal <|> numVal <|> stringVal)
  where
    tokenVal = _const "true" <|> _const "false"
    stringVal = _string
    leftAndRight [] Nothing = unparsable
    leftAndRight [] (Just ds) = return $ "0." ++ dig2str ds
    leftAndRight ls Nothing = return $ dig2str ls
    leftAndRight ls (Just rs) = return $ dig2str ls ++ "." ++ dig2str rs
    dig2str ds = concat [ [d] | d <- ds ]
    numVal = do
      neg <- optional $ symbol "-"
      left <- many digit 
      right <- optional $ do
        symbol "."
        some digit
      leftAndRight left right


_readonly :: Parser Bool
_readonly = (/=Nothing) <$> (optional . token $ symbol "readonly")

_fieldName :: Parser String
_fieldName = _varName <|> do
  full <- _string

  let name = drop 1 . take (length full - 1) $ full -- strip the quotes
  guardWith ("field `"++ name ++"` contains illegal character '/'") (all (/='/') name)
  guardWith ("field `"++ name ++"` cannot consist entirely of periods") (any (/='.') name)
  guardWith ("field `"++ name ++"` cannot match __.*__") (not (length name > 3 && take 2 name == "__" && drop (length name - 2) name == "__"))
  return full

  


_field :: Parser Field
_field = do
  readonly <- _readonly
  name <- token _fieldName
  opt <- optional $ symbol "?"
  symbol ":"
  isConst <- optional $ symbol "const"
  types <- _typeRefUnion
  guardWith ("field `"++ name ++"` lacks a type" ) (length types > 0)
  return $ Field (opt == Nothing) name types (isConst /= Nothing || readonly)

_topLevelType :: Parser TopLevel
_topLevelType = do 
  symbol "type"
  name <- require "type name missing" $ token _varName
  require "missing `=` after type name" $ symbol "="
  members <- _typeRefUnion
  guardWith ("type `"++ name ++"` is missing definition" ) (length members > 0)
  optional $ symbol ";"
  return $ TopLevelType name members

_pathStatic :: Parser PathPart
_pathStatic = do
  start <- _alphaNum <|> charIn "_$"
  rest <- many $ _alphaNum <|> charIn "_$-:"
  return $ PathPartStatic (start:rest)

_pathVar :: Parser PathPart
_pathVar = do
  n <- grouped "{" "}" _varName 
  return $ PathPartVar n

_pathWild :: Parser PathPart
_pathWild = grouped "{" "}" $ do
  v <- _varName
  string "=**"
  return $ PathPartWild v

_pathParts :: Parser [PathPart]
_pathParts = manywith (char '/') (_pathVar <|> _pathStatic <|> _pathWild)

_pathType :: Parser [TypeRef]
_pathType = do
  symbol "is"
  refs <- token _typeRefUnion
  return refs

_pathDir :: Parser PathDirective
_pathDir = do
  symbol "allow"
  ops <- manywith (symbol ",") $ enum 
    [ "read"
    , "get"
    , "list"
    , "write"
    , "create"
    , "update"
    , "delete" ]
  guardWith "must provide at least one operation in a path directive" (length ops > 0)
  e <- optional explicit
  let dir = PathDirective ops $ maybe "true" id e
  optional $ symbol ";"
  return dir
  where 
    explicit = do
      symbol ":"
      optional $ symbol "if"
      optional space
      body <- token _expr 
      return body

_pathBodyFunc = PathBodyFunc <$> _funcDef
_pathBodyPath = PathBodyPath <$> _path
_pathBodyDir = PathBodyDir <$> _pathDir

_path :: Parser PathDef
_path = do
  symbol "match"
  optional $ symbol "/"
  parts <- require "expected a path after `match`" $ token _pathParts
  className <- (_pathType <|> return [])
  require "expected a `{`" $ symbol "{"
  body <- many $ _pathBodyDir <|> _pathBodyFunc <|> _pathBodyPath
  require "expected a closing `}`" $ symbol "}"
  return  $ PathDef parts className body

_topLevel = _topLevelOptVar
        <|> (TopLevelPath <$> _path) 
        <|> _topLevelType 
        <|> TopLevelFunc <$> _funcDef

parseRules :: String -> ParserResult [TopLevel]
parseRules = apply (many _topLevel ) . trim
