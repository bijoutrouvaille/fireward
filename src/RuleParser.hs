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

data TopLevel = TopLevelType String [TypeRef]
              | TopLevelPath PathDef
              | TopLevelFunc FuncDef
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

-- TopLevelType String [TypeRef]
data TypeDef = TypeDef [Field] deriving (Show, Eq)
-- TypeNameRef name (Maybe array-size)
data TypeRef = TypeNameRef String (Maybe Int) | InlineTypeRef TypeDef deriving (Show, Eq)
-- Field Required Name [TypeRef] Constant
-- data Field = Field Bool String [TypeRef] Bool deriving (Show, Eq)
data Field = Field
           { required :: Bool
           , fieldName :: String
           , typeRefs :: [TypeRef]
           , constant :: Bool
           } deriving (Show, Eq)

           -- , constant: Bool

_alpha = lower <|> upper
_alphaNum = _alpha <|> digit
_varStart = _alpha <|> oneOf "_$"
_varName = do
  c <- _varStart
  rest <- many (_alphaNum <|> oneOf "_$")
  return (c:rest)

separated = manywith . symbol

escape :: Char -> Parser String
escape c = do
  char '\\'
  r <- sat (==c)
  return ('\\':[r])


{-
- str = "(all - {"})*"
-}
_getWhile p = many $ sat p
_stringD :: Char -> Parser String
_stringD delim = do
  char delim
  a <- many $ (_const ('\\':[delim]) <|> (:[]) <$> sat (/=delim))
  char delim
  return $ concat (([delim]:a) ++ [[delim]])

_string :: Parser String
_string = _stringD '"' <|> _stringD '\''

readDef :: (Read a) => a -> String -> a
readDef def s = case reads s of
              [(x, "")] -> x
              _ -> def

_natural :: Parser Int
_natural = do  -- a natural number
  str <- some digit
  guard (str/="")
  let n = readDef (-1) str
  guardWith "expected an integer" (n /= -1)
  return n


_funcBody :: Parser String
_funcBody = token $ do
  let notDone c = c/='}' && c/=';'
  a <- many $ _string <|> satS notDone
  optional $ symbol ";"
  return $ concat a


_funcDef :: Parser FuncDef
_funcDef = do
  symbol "function"
  name <- token _varName
  params <- grouped "(" ")" paramList
  symbol "{"
  optional $ symbol "return"
  body <- _funcBody
  symbol "}"
  return $ FuncDef name params (trim body)
  where paramList = separated "," (token _varName)

_typeDef :: Parser TypeDef
_typeDef = grouped "{" "}" $ do
  members <- many _field
  return $ TypeDef members
  
_terminated parser terminator = do
  v <- parser
  terminator
  return v
_typeRefs :: Parser [TypeRef]
_typeRefs = manywith (symbol "|") ( 
  -- (TypeNameRef <$> withComma (token _varName)) 
  (withComma _singleTypeName)
    <|> (InlineTypeRef <$> withComma _typeDef))
  where
    comma = optional $ symbol ","
    withComma p = _terminated p comma

_listOp :: Parser Int
_listOp = do
  char '['
  size <- optional _natural
  char ']'
  return $ maybe 0 id size
_singleTypeName = do
  name <- token _varName
  arrSize <- optional $ _listOp
  return $ TypeNameRef name arrSize

_field :: Parser Field
_field = do
  name <- token _varName
  opt <- optional $ symbol "?"
  symbol ":"
  isConst <- optional $ symbol "const"
  types <- _typeRefs
  return $ Field (opt == Nothing) name types (isConst /= Nothing)

_topLevelType :: Parser TopLevel
_topLevelType = do 
  symbol "type"
  name <- token _varName
  symbol "="
  members <- _typeRefs
  return $ TopLevelType name members

_pathStatic :: Parser PathPart
_pathStatic = do
  n <- _varName
  return $ PathPartStatic n

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
_pathType = (symbol "is" >> token _typeRefs) <|> return []

_pathDir :: Parser PathDirective
_pathDir = do
  symbol "allow"
  -- ops <-  _const "read"
  ops <- manywith (symbol ",") $ enum 
    [ "read"
    , "get"
    , "list"
    , "write"
    , "create"
    , "update"
    , "delete" ]
  symbol ":"
  optional $ symbol "if"
  space
  body <- many $ sat (/=';') 
  char ';'
  return $ PathDirective ops body

_path :: Parser PathDef
_path = do
  symbol "match"
  optional $ symbol "/"
  parts <- require "expected a path after `match`" $ token _pathParts
  className <- _pathType
  require "expected a `{`" $ symbol "{"
  body <- many (PathBodyPath <$> _path <|> PathBodyDir <$> _pathDir <|> PathBodyFunc <$> _funcDef)
  require "expected a closing `}`" $ symbol "}"
  return  $ PathDef parts className body


  
_topLevel = (TopLevelPath <$> _path) 
        <|> _topLevelType 
        <|> TopLevelFunc <$> _funcDef

parseRules :: String -> ParserResult [TopLevel]
parseRules = apply (many _topLevel ) . trim
