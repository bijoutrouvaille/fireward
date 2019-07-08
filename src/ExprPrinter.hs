{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ExprPrinter ( 
  printExpr
) where

import ExprParser
import Data.List (intercalate)

commajoin = intercalate ", "

printExpr :: Expr -> String
printExpr (ExprGrp expr) = "(" ++ printExpr expr ++ ")"
printExpr (ExprBin op left right) = printExpr left ++ " " ++ show op ++ " " ++ printExpr right
printExpr (ExprCall funcName exprs) = funcName ++ "(" ++ printArr ", " exprs ++ ")"
printExpr (ExprVar var) = var
printExpr (ExprUn op expr) = show op ++ printExpr expr
printExpr (ExprStr s) = "\"" ++ s ++ "\""
printExpr (ExprFloat num) = show num
printExpr (ExprInt num) = show num
printExpr (ExprBool True) = "true"
printExpr (ExprBool False) = "false"
printExpr (ExprNull) = "null"
printExpr (ExprIndexed e i r) = printExpr e ++ "[" ++ printExpr i ++ printIxRange r ++ "]"
printExpr (ExprPath p) = "(" ++ p ++ ")"
printExpr (ExprList es) = "[" ++ (commajoin [ printExpr e | e <- es ]) ++ "]"
printExpr (ExprMap kvs) = "{ " ++ (commajoin [ printKeyVal kv | kv <- kvs]) ++ " }"

printKeyVal (s, e) = s ++ ": " ++ printExpr e
printIxRange Nothing = ""
printIxRange (Just r) = ":" ++ printExpr r
printArr sep es = intercalate sep [ printExpr e | e <- es ]
