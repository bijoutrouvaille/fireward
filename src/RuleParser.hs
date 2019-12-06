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
, _funcBody
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

data FuncDef = FuncDef String [String] String deriving (Show, Eq)

data TypeDef = TypeDef [Field] deriving (Show, Eq)

-- TypeNameRef name-of-the-type (Maybe array-size) -- Nothing if not array or not array not finite
data TypeRef = TypeNameRef String (Maybe Int) 
             | InlineTypeDef TypeDef 
             | LiteralTypeRef String
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

_funcBody :: Parser String
_funcBody = token $ do
  let notDone c = c/='}' && c/=';' && c/='"'
  a <- many $ _string <|> satS notDone
  optional $ symbol ";"
  return . trim $ concat a

      

_expr :: Parser String
_expr = do
  e <- ExprParser.expr
  return $ ExprPrinter.printExpr e

_funcDef :: Parser FuncDef
_funcDef = do
  symbol "function"
  name <- require "missing function name" $ token _varName
  params <- require ("function `"++name++"` is missing the parameter list") $ 
    grouped "(" ")" paramList
  require ("function `"++name++"` is missing an opening `{`") $ 
    symbol "{"

  optional $ symbol "return"
  body' <- optional $ token _expr 
  let body = maybe "" id body'

  optional $ symbol ";"

  require ("function `"++name++"` is missing a closing `}`") $ 
    symbol "}"

  guardWith ("function `"++name++"` is missing a body") (length body > 0)

  return $ FuncDef name params (trim body)

  where paramList = separated "," (token _varName)

_typeDef :: Parser TypeDef
_typeDef = grouped "{" "}" $ do
  members <- many _field
  return $ TypeDef members
  
_typeRefs :: Parser [TypeRef]
_typeRefs = manywith (symbol "|") 
  (   (withComma _literalTypeRef)
  <|> (withComma _singleTypeName)
  <|> (InlineTypeDef <$> withComma _typeDef)
  )
  where
    withComma p = _terminated p comma
    comma = optional $ symbol ","

_listOp :: Parser Int
_listOp = do
  symbol "["
  size <- optional $ token _natural
  require "expected closing `]`" $ symbol "]"
  return $ maybe 0 id size
_singleTypeName = do
  name <- token _varName
  arrSize <- optional $ _listOp
  return $ TypeNameRef name arrSize
_literalTypeRef :: Parser TypeRef
_literalTypeRef =
  LiteralTypeRef <$> token (tokenVal <|> numVal <|> stringVal)
  where
    tokenVal = _const "true" <|> _const "false"
    numVal = show <$> _float
    stringVal = _string


_field :: Parser Field
_field = do
  name <- token _varName
  opt <- optional $ symbol "?"
  symbol ":"
  isConst <- optional $ symbol "const"
  types <- _typeRefs
  guardWith ("field `"++ name ++"` lacks a type" ) (length types > 0)
  return $ Field (opt == Nothing) name types (isConst /= Nothing)

_topLevelType :: Parser TopLevel
_topLevelType = do 
  symbol "type"
  name <- require "type name missing" $ token _varName
  require "missing `=` after type name" $ symbol "="
  members <- _typeRefs
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
  refs <- token _typeRefs
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
