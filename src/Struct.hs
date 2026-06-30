module Struct where

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | String String
             | Bool Bool

data Env = Env {
       defuns :: [String]
     , generated :: String
}

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "true"
showVal (Bool False) = "false"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
