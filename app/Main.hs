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
out output (Left e) = hPutStrLn stderr (showErr e)
out output (Right v) = return v >>= output 

main :: IO ()
main = do
  (opts, actions, nonOptions, errors) <- getOptions
  let Options { optInput = input
              , optOutput = output
              , optVerbose = verbose
              } = opts

  input >>= (return . generate) >>= out output

