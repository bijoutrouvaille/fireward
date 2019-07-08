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


out output (Left e) = hPutStrLn stderr e
out output (Right v) = output v

generate lang
  | lang == "typescript" = TSGenerator.generate
  | lang == "ts" = TSGenerator.generate
  | lang == "rules" = RuleGenerator.generate True
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

