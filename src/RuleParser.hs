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
  let n = readDef (-1) str
  guardWith "expected an integer" (n /= -1)
  return n


_funcBody :: Parser String
_funcBody = token $ do
  let notDone c = c/='}' && c/=';' && c/='"'
  a <- many $ _string <|> satS notDone
  optional $ symbol ";"
  return . trim $ concat a


_funcDef :: Parser FuncDef
_funcDef = do
  symbol "function"
  name <- require "missing function name" $ token _varName
  params <- require ("function `"++name++"` is missing the parameter list") $ grouped "(" ")" paramList
  require ("function `"++name++"` is missing an opening `{`") $ symbol "{"
  optional $ symbol "return"
  body <- _funcBody
  require ("function `"++name++"` is missing a closing `}`") $ symbol "}"
  guardWith ("function `"++name++"` is missing a body") (length body > 0)
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
  (withComma _singleTypeName)
    <|> (InlineTypeRef <$> withComma _typeDef))
  where
    comma = optional $ symbol ","
    withComma p = _terminated p comma

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
_pathType = do
  symbol "is"
  refs <- token _typeRefs
  -- guardWith "expected a path type" (length refs > 0)
  -- guardWith "expected a path type" (refs /= [InlineTypeRef (TypeDef [])]) -- the above really does not work
  return refs
-- (symbol "is" >> (require "expected a path type" $ token _typeRefs)) <|> return []

_expr :: [Char] -> Parser String
_expr end = do
  fmap concat . many $ _string <|> satS f
  where f c = not . elem c $ concat [end, ";'\""]--c/=';' && c/='"' && c/='}' && c/='\''

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
  require "path directive is missing a `:`" $ symbol ":"
  optional $ symbol "if"
  optional space
  body <- _expr "};" -- many $ sat (/=';') 
  optional $ char ';'
  return $ PathDirective ops body

_path :: Parser PathDef
_path = do
  symbol "match"
  optional $ symbol "/"
  parts <- require "expected a path after `match`" $ token _pathParts
  className <- (_pathType <|> return [])
  require "expected a `{`" $ symbol "{"
  body <- many (PathBodyPath <$> _path <|> PathBodyDir <$> _pathDir <|> PathBodyFunc <$> _funcDef)
  require "expected a closing `}`" $ symbol "}"
  -- guardWith "expected a path type" (refs /= [InlineTypeRef (TypeDef [])]) -- the above really does not work
  return  $ PathDef parts className body


  
_topLevel = (TopLevelPath <$> _path) 
        <|> _topLevelType 
        <|> TopLevelFunc <$> _funcDef

parseRules :: String -> ParserResult [TopLevel]
parseRules = apply (many _topLevel ) . trim
