module Loc
  ( loc
  , Loc (..)
  ) where

data Loc = Loc Int String
  deriving (Show, Eq)

loc :: String -> String -> Maybe Loc
loc source remainder = result
  where
    result :: Maybe Loc
    result = lineDiff >>= colDiff
    lineDiff :: Maybe (String, [String], Int)
    lineDiff = diff (lines source) (lines remainder)
    colDiff :: (String, [String], Int) -> Maybe Loc
    colDiff (head, tail, n) = Just $ Loc n head
      
diff :: Eq a => [a] -> [a] -> Maybe (a,[a], Int)
diff s r = result where
  result = if s==r then Nothing else next s r 0
  next [] r n = Nothing
  next (s:ss) [] n = Just (s, ss, n)
  next (s:ss) (r:rr) n = if s/=r && ss/=rr
                         then next ss (r:rr) (n+1)
                         else Just (s, ss, n)
