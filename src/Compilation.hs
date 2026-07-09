module Compilation where

import Codegen
import Lexer
import Struct
import Text.ParserCombinators.Parsec (parse, many1)
import Text.Parsec.Error

parseCode :: String -> Either ParseError [LispVal]
parseCode code = parse (many1 parseExpr) "lisp" code

showParse :: Either ParseError [LispVal] -> IO ()
showParse (Left x) = putStrLn $ show x
showParse (Right x) = sequence_ $ map putStrLn $ map show x

compileCode :: String -> String
compileCode code = case parseCode code of
                 Right val -> (codegen emptyEnv val)
                 Left _ -> ""
