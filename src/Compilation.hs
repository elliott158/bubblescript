module Compilation where

import Codegen
import Lexer
import Struct
import System.IO
import Text.ParserCombinators.Parsec (parse)

compileCode :: String -> String
compileCode code = case parse parseExpr "lisp" code of
                 Right val -> (codegen val)
                 Left _ -> ""

codeOutput :: String -> (LispVal -> String) -> IO ()
codeOutput code f = do
           case parse parseExprs "lisp" code of
                Right val -> (putStrLn $ concat (map f val))
                Left err -> (hPutStrLn stderr ("Error: " ++ show err))

parseCodePrint :: String -> IO ()
parseCodePrint code = codeOutput code show

compileCodePrint :: String -> IO ()
compileCodePrint code = codeOutput code codegen