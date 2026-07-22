{-# LANGUAGE FlexibleInstances #-} 

module Compilation where

import Codegen
import Lexer
import Struct
import Text.Megaparsec.Error

compileCode :: String -> Env
compileCode code = case parseCode code of
                 Right val -> (codegen emptyEnv val)
                 Left er -> error (errorBundlePretty er)
