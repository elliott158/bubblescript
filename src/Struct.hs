module Struct where

data Stmt = Atom String
          | List [Stmt]
          | Number Integer
          | String String
          | Bool Bool

data Env = Env {
       generated :: String
     , inputEnv :: Maybe Stmt
     , includes :: [String]
}

emptyEnv :: Env
emptyEnv = Env {generated = "", inputEnv = Nothing, includes = []}

instance Show Env where show = generated

instance Show Stmt where show = showVal

showVal :: Stmt -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "true"
showVal (Bool False) = "false"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"

unwordsList :: [Stmt] -> String
unwordsList = unwords . map showVal
