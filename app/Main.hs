module Main where


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

out output (Left e) = hPutStrLn stderr e
out output (Right v) = output v

generate lang
  | lang == "typescript" = TSGenerator.generate
  | lang == "ts" = TSGenerator.generate
  | lang == "rules" = fmap wrapRules . RuleGenerator.generate 
  | otherwise = const . Left $ 
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

