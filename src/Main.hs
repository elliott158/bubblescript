module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Data.List ((!?))

import Compilation
import Struct
import Codegen

getOutputFileName :: String -> String
--trims off the file extension and replaces it with .hs
getOutputFileName file = concat [(fst $ span (/= '.') file), ".hs"]

outputCompiled :: String -> Env -> IO ()
outputCompiled file compiled = do
               included <- includeFiles compiled
               let includedCode = show $ compileCode included
               let toWrite = getOutputFileName file
               writeFile toWrite $ concat [includedCode, show compiled]

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
          outputCompiled file $ compileCode code
