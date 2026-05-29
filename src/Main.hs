module Main where

import System.Environment (getArgs)

import Compilation

main :: IO ()
main = do
  code <- getContents
  args <- getArgs
  if ("-p" `elem` args) then (parseCodePrint code) else (compileCodePrint code)
