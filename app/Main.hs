module Main where


import Error (Error(..))
import Loc (Loc(..))
import qualified RuleGenerator 
import qualified TSGenerator 

import OptionParser (getOptions, Options(..))

import System.IO 
  ( IO(..)
  , readFile
  , writeFile
  , hPutStrLn
  , stderr
  , stdin
  )
import System.Exit (exitWith, ExitCode(..))
import Data.List (intercalate)
import Data.Char (isSpace)

showLoc Nothing = ""
showLoc (Just (Loc l c)) = " (on line "++show l++": "++c++")"
showErr (Error loc text) = unlines
  [ "Error"++showLoc loc++":"
  , text
  ]

shift n = take (n*2) $ repeat ' '
indentBy n = unlines . fmap (shift n ++) . lines

wrapRules r = intercalate "\n" 
  [ "service cloud.firestore {"
  , "  match /databases/{database}/documents {"
  , ""
  , indentBy 2 r
  , "  }"
  , "}"
  ]
-- rulesM = return . wrapRules
out output (Left e) = hPutStrLn stderr (showErr e)
out output (Right v) = output v

generate lang
  | lang == "typescript" = TSGenerator.generate
  | lang == "ts" = TSGenerator.generate
  | lang == "rules" = fmap wrapRules . RuleGenerator.generate 
  | otherwise = const . Left . Error Nothing $ 
      "Specified language \""++lang++"\"not recognized."

main :: IO ()
main = do
  (opts, actions, nonOptions, errors) <- getOptions
  let Options { optInput = input
              , optOutput = output
              , optVerbose = verbose
              , optLang = lang
              } = opts

  input >>= (return . generate lang) >>= out output

