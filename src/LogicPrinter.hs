module LogicPrinter
  ( Expr(..)
  , Op(..)
  ) where

import Data.Char (toUpper)
import Data.List (findIndices, intercalate, stripPrefix)
import Prelude hiding (showLogic)


data Op = And | Or deriving (Eq)
instance Show Op where
  show And = "&&"
  show Or = "||"

data Expr = Term Op [Expr] | Atom String | Group Expr
instance Show Expr where
  show = p 0


shift = 2
line ind = "\n" ++ indent ind
indent n = take (max 0 n) $ repeat ' '


p :: Int -> Expr -> String
p ind (Atom term) = term
p ind (Group (Atom term)) = term
p ind (Group e) = "(\n" ++ p (ind + shift) e ++ "\n" ++ indent ind ++ ")"
p ind (Term op es)
-- (A & (B & (C | (D | E)))) & (F | G)
--  A &  B & (C |  D | E  )  & (F | G)
  | parens = "( " ++ s ++ line ind ++ ")"
  | otherwise = s
  where
    ind' | parens = ind + 2 | otherwise = ind
    shift' | parens = 2 | otherwise = 0
    -- shift' | op == Or = shift | otherwise = 0
    s = intercalate (line ind' ++ show op ++ " ") $ p (ind + shift') <$> es
    parens = length es > 1 && op==Or

{-
assiciativity :: bool
precedence :: int
-}
