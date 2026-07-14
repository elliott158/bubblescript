{-# LANGUAGE FlexibleInstances #-} 

module Compilation where

import Codegen
import Lexer
import Struct
import Text.ParserCombinators.Parsec (parse)
import Text.Parsec.Error

parseCode :: String -> Either ParseError [LispVal]
parseCode code = parse parseExprs "lisp" code

showParse :: Either ParseError [LispVal] -> String
showParse (Left x) = show x
showParse (Right x) = concat $ map show x

compileCode :: String -> Env
compileCode code = case parseCode code of
                 Right val -> (codegen emptyEnv val)
                 Left _ -> emptyEnv
