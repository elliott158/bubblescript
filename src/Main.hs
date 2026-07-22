module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Data.List ((!?))

import Compilation
import Struct
import Codegen

outputCompiled :: Env -> IO ()
outputCompiled compiled = do
               included <- includeFiles compiled
               let includedCode = show $ compileCode included
               putStrLn $ concat [includedCode, show compiled]

usage :: String
usage = "Usage:\nbubble [options] [input files]"

dieUsage :: String -> IO ()
dieUsage x = die $ concat [x, "\n", usage]

main :: IO ()
main = do
  args <- getArgs
  case (args !? 0) of
       (Nothing) -> dieUsage "Not enough arguments."
       (Just file) -> do
          code <- readFile file
          outputCompiled $ compileCode code
