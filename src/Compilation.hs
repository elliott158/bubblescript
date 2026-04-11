module Compilation where

import Codegen
import Lexer
import System.IO
import Text.ParserCombinators.Parsec (parse)

compileCode :: String -> String
compileCode code = case parse parseExpr "lisp" code of
                 Right val -> (codegen val)
                 Left _ -> ""

parseCodePrint :: String -> IO ()
parseCodePrint code = do
  case parse parseExpr "lisp" code of
    Right val -> (putStrLn (show val))
    Left err -> (hPutStrLn stderr ("Error: " ++ show err))

compileCodePrint :: String -> IO ()
compileCodePrint code = do
  case parse parseExpr "lisp" code of
    Right val -> (putStrLn (codegen val))
    Left err -> (hPutStrLn stderr ("Error: " ++ show err))
