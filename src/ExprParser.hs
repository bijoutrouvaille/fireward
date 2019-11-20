module ExprParser (
  expr, -- the main combinator
  Expr(..),
  BinOp(..),
  UnOp(..),
  PathPart(..),
  FuncCall(..)
) where

{-
- Reference Shortcuts
- https://firebase.google.com/docs/rules/rules-language#firestore
- https://firebase.google.com/docs/reference/rules/rules
-}

import Debug.Trace (trace)
import Parser
import Control.Applicative (optional, empty)
import Data.Char (isSpace)
import Data.List (intercalate)
import Parser
import Combinators

data UnOp = OpNeg | OpPos | OpBang
  deriving (Eq)
allunops = [ OpNeg , OpPos , OpBang ]
instance Show UnOp where
  show OpNeg = "-"
  show OpPos = "+"
  show OpBang = "!"

data BinOp = OpAnd 
           | OpOr 
           | OpPlus 
           | OpMinus 
           | OpMult
           | OpDiv 
           | OpDot
           | OpIs
           | OpIn
           | OpMod
           | OpGt
           | OpNe
           | OpEq
           | OpGte
           | OpLt
           | OpLte
  deriving (Eq)

allbinops = 
  [ OpOr
  , OpAnd
  , OpPlus
  , OpMinus
  , OpMult
  , OpDiv
  , OpDot
  , OpIs
  , OpIn
  , OpMod
  , OpGt
  , OpNe
  , OpEq
  , OpGte
  , OpLt
  , OpLte
  ]
instance Show BinOp where
  show OpOr = "||"
  show OpAnd = "&&"
  show OpPlus = "+"
  show OpMinus = "-"
  show OpMult = "*"
  show OpDiv = "/"
  show OpDot = "."
  show OpIs = "is"
  show OpIn = "in"
  show OpMod = "%"
  show OpEq = "=="
  show OpNe = "!="
  show OpGt = ">"
  show OpGte = ">="
  show OpLt = "<"
  show OpLte = "<="

data FuncCall = FuncCall String [Expr] deriving (Show, Eq)
data PathPart = PathPartLit String | PathPartExpr Expr
              deriving (Show, Eq)
data Expr = ExprGrp Expr 
          | ExprBin BinOp Expr Expr 
          | ExprCall FuncCall
          | ExprVar String 
          | ExprUn UnOp Expr 
          | ExprStr String 
          | ExprFloat Float 
          | ExprInt Int
          | ExprBool Bool 
          | ExprNull
          | ExprIndexed Expr Expr (Maybe Expr)
          | ExprPath [PathPart]
          | ExprList [Expr]
          | ExprMap [(String, Expr)]
          deriving (Show, Eq)

toInt :: Float -> Int
toInt = round
isInt :: Float -> Bool
isInt x = x == fromInteger (round x)


num = do
  neg <- optional $ symbol "-"
  optional space
  left <- fromIntegral <$> _natural 
  decMaybe <- optional _dec
  let decStr = maybe "0" id decMaybe :: String
  let dec = readDef 0 decStr :: Float
  let right = shiftDec dec
  let val = (left + right) * (if neg==Nothing then 1 else -1)
  let ex = if decMaybe == Nothing then ExprInt (toInt val) else ExprFloat val
  return ex 
  where 
        shiftDec x = if x >= 1 then shiftDec (x/10) else x
        _dec = do
          symbol "."
          some digit


expr :: Parser Expr
expr = do
  e <- _expr
  optional $ symbol ";"
  return e
  where 
    _expr = unit
    gp = do -- group
      symbol "("
      e <- unit
      require "expected a closing ')'" $ symbol ")"
      return $ ExprGrp e

    rawpathlit :: Parser Expr
    rawpathlit = do

      let pathExpr = PathPartExpr <$> grouped "$(" ")" _expr
      let pathLit = fmap PathPartLit . some $ _alpha <|> digit <|> oneOf "_-" 

      let sep = symbol "/"
       

      token sep
      parts <- somewith sep (token pathLit <|> token pathExpr)
      guardWith "path must contain at least 1 part" (length parts > 0)

      return $ ExprPath parts
      
    pathlit :: Parser Expr
    pathlit = do
      symbol "("
      p <- rawpathlit
      require "raw path is missing a closing paren `)`" $ symbol ")"
      return p

    baddies = 
      altr [ symbol b >> failWith ("symbol " ++ b ++ " not allowed.") 
             | b <- [ "#", "~", "===", "++", "--", "**" ] ]

      
    unop = do
      optional space
      optional baddies
      op <- altr [ symbol (show o) >> return o | o <- allunops ]
      e <- unit
      return $ ExprUn op e
      
    indexer = do
      symbol "["
      e <- require "index missing expression after [" _expr
      r <- optional indexerRange
      require "index missing closing bracket `]`" $ symbol "]"
      return (e, r)

    indexerRange = do
      symbol ":"
      require "index range is missing the second part" _expr


    listlit = do
      symbol "["
      e <- manywith (symbol ",") (token _expr)
      require "list missing closing bracket `]`" $ symbol "]"
      return $ ExprList e

    maplit = do
      symbol "{"
      e <- manywith (symbol ",") keyval
      require "list missing closing brace `}`" $ symbol "}"
      return $ ExprMap e
      where 
        val = symbol ":" >> require "map object missing a value after `:`" _expr
        quote x = "\"" ++ x ++ "\""
        rawkey = quote <$> some (_alphaNum <|> oneOf "_")
        strkey = _stringD '"'
        key = rawkey <|> strkey
        keyval = (,) <$> token key <*> val


    funcCall = token $ do
      optional space
      namePath <- somewith (symbol ".") _varName
      let name = intercalate "." namePath
      symbol "("
      params <- manywith (symbol ",") (token funcParam)
      require "function call expected a closing paren `)`" $ symbol ")"
      return $ FuncCall name params
      where funcParam = rawpathlit <|> _expr

    unit = binOrExpr where
      atom = baddies 
        <|> maplit 
        <|> listlit 
        <|> pathlit 
        <|> unop 
        <|> gp 
        <|> num 
        <|> string 
        <|> bool 
        <|> nil 
        <|> func <|> var 

      binOrExpr = do
        left <- possiblyIndexed
        optional baddies
        op <- optional $ altr [ symbol (show o) >> return o | o <- allbinops ]
        let rightP op = require ("could not parse right side of operator " ++ show op) (token binOrExpr)
        let result Nothing = return left
            result (Just op) = ExprBin op left <$> rightP op
        result op

      possiblyIndexed = do
        e <- atom
        mix <- optional indexer
        let res Nothing = e
            res (Just (i, r)) = ExprIndexed e i r
        return $ res mix

    nil = token $ symbol "null" >> return ExprNull
    bool = token $ (symbol "true" >> return (ExprBool True))
                <|> (symbol "false" >> return (ExprBool False))
    string = ExprStr <$> _string
    
    var :: Parser Expr
    var = do 
      optional space
      x <- somewith (symbol ".") _varName
      return $ ExprVar (intercalate "." x) 
    func = token $ do
      f <- funcCall
      return $ ExprCall f


      


