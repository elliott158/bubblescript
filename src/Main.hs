module Main where

import System.Environment (getArgs)

import Compilation
import Struct
import Codegen

outputCompiled :: Env -> IO ()
outputCompiled compiled = do
               included <- includeFiles compiled
               let includedCode = show $ compileCode included
               putStrLn $ concat [includedCode, show compiled]

main :: IO ()
main = do
  code <- getContents
  args <- getArgs
  if ("-p" `elem` args)
  then (putStrLn $ showParse $ parseCode code)
  else (outputCompiled $ compileCode code)
