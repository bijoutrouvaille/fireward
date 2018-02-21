module Main where


import RuleGenerator (generate, Error(..), Loc(..))
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
showLoc (Just (Loc l c)) = " (line: "++show l++", col: "++show c++")"
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
rulesM = return . wrapRules
out output (Left e) = hPutStrLn stderr (showErr e)
out output (Right v) = rulesM v >>= output 

main :: IO ()
main = do
  (opts, actions, nonOptions, errors) <- getOptions
  let Options { optInput = input
              , optOutput = output
              , optVerbose = verbose
              } = opts

  input >>= (return . generate) >>= out output

