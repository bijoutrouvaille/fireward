module ExprParser (
  expr, -- the main combinator
  Expr(..),
  BinOp(..),
  UnOp(..),
) where

import Debug.Trace (trace)
import Parser
import Control.Applicative (optional, empty)
import Data.Char (isSpace)
import Data.List (intercalate)
import Parser
import Combinators

data UnOp = OpNeg | OpPos | OpBang
  deriving (Show, Eq)

data BinOp = OpAnd 
           | OpOr 
           | OpPlus 
           | OpMinus 
           | OpMult
           | OpDiv 
           | OpDot
           | OpIs
           | OpIn
           | OpGt
           | OpEq
           | OpEqs
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
  , OpGt
  , OpEq
  , OpEqs
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
  show OpEq = "=="
  show OpEqs = "==="
  show OpGt = ">"
  show OpGte = ">="
  show OpLt = "<"
  show OpLte = "<="




data Expr = ExprGrp Expr 
          | ExprBin BinOp Expr Expr 
          | ExprCall String [Expr] 
          | ExprVar String 
          | ExprUn UnOp Expr 
          | ExprStr String 
          | ExprFloat Float 
          | ExprInt Int
          | ExprBool Bool 
          | ExprNull
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
  e <- unit
  optional $ symbol ";"
  return e
  where 
    gp = do
      symbol "("
      e <- unit
      require "expected a closing ')'" $ symbol ")"
      return $ ExprGrp e
 


    binops = let bin :: [BinOp] -> Parser Expr
                 bin [] = empty
                 bin (o:os) = binop o <|> bin os
              in bin allbinops :: Parser Expr

    binop :: BinOp -> Parser Expr
    binop op = do
      left <- atom
      symbol (show op)
      right <- unit
      return (ExprBin op left right)
    
    atom = gp <|> num <|> string <|> bool <|> nil <|> func <|> var 
    unit :: Parser Expr 
    unit = binops <|> atom

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
      optional space
      namePath <- somewith (symbol ".") _varName
      let name = intercalate "." namePath
      symbol "("
      params <- manywith (symbol ",") unit
      require "expected a closing paren `)`" $ symbol ")"
      return $ ExprCall name params


      


