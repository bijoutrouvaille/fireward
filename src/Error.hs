module Error 
  ( Error (..)
  ) where


import Loc (Loc)
data Error = Error (Maybe Loc) String
         deriving (Show, Eq)

