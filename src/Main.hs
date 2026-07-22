module Main where

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
  outputCompiled $ compileCode code
