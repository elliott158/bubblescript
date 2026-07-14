module Main where

import System.Environment (getArgs)

import Compilation
import Struct
import Codegen

outputCompiled :: Env -> IO ()
outputCompiled compiled = do
               included <- includeFiles compiled
               putStrLn $ concat [included, show compiled]

main :: IO ()
main = do
  code <- getContents
  args <- getArgs
  let parsed = parseCode code
  let compiled = compileCode code
  if ("-p" `elem` args)
  then (putStrLn $ showParse $ parsed)
  else (outputCompiled compiled)
