module Codegen (codegen, includeFiles) where

import Struct

makeOp :: Env -> String -> [LispVal] -> String
makeOp env op args = concat [showHsEnv env (args !! 0), " ", op, " ", showHsEnv env (args !! 1)]

wrapParens :: String -> String
wrapParens x = "(" ++ x ++ ")"

showArgs :: Env -> LispVal -> String
showArgs env (List (x:xs)) = concat [" ", showHsEnv env x, concat $ map (showArgs env) xs]
showArgs _ _ = ""

defun :: Env -> [LispVal] -> String
defun env args = concat [showHsEnv env (args !! 0), showArgs env (args !! 1), defunTail]
      where
        defunTail = concat [" = ", showHsEnv env (args !! 2), "\n"]

addIncludes :: Env -> String -> Env
addIncludes env x = env { includes = (includes env) ++ [x] }

outputEnv :: Env -> String -> Env -- set output of env
outputEnv env x = env {generated = x}

setInputEnv :: Env -> LispVal -> Env
setInputEnv env x = env {inputEnv = Just x}

showHsEnvE :: Env -> LispVal -> Env
showHsEnvE env x = showHs $ setInputEnv env x

showHsEnv :: Env -> LispVal -> String
showHsEnv env x = generated $ showHsEnvE env x

includeFiles :: Env -> IO String
includeFiles env = do
             concat <$> mapM (\fp -> (++ "\n") <$> readFile fp) (includes env)

mapShowHs :: Env -> [LispVal] -> [String]
mapShowHs env (x:xs) = showHsEnv env x : mapShowHs env xs
mapShowHs _ [] = []

concatEnv :: [Env] -> Env
concatEnv xs = Env { generated = concat $ map generated xs
                   , inputEnv = Nothing
                   , includes = concat $ map includes xs
                   }

mapShowHsEnv :: Env -> [LispVal] -> [Env]
mapShowHsEnv env (x:xs) = showHsEnvE env x : mapShowHsEnv env xs
mapShowHsEnv _ [] = []

unwrapInputEnv :: Env -> LispVal
unwrapInputEnv env = case (inputEnv env) of
               Just x -> x
               Nothing -> error "Env without input."

showHs :: Env -> Env
showHs env = case (unwrapInputEnv env) of
       (Atom x) -> outputEnv env $ x
       (Number x) -> outputEnv env $ show x
       (String x) -> outputEnv env $ "\"" ++ x ++ "\""
       (Bool x) -> outputEnv env $ if x then "true" else "false"
       (List []) -> outputEnv env $ ""

       (List (op:args)) -> case op of
             (Atom "+") -> outputEnv env $ wrapParens $ makeOp env "+" args
             (Atom "-") -> outputEnv env $ wrapParens $ makeOp env "-" args
             (Atom "*") -> outputEnv env $ wrapParens $ makeOp env "*" args
             (Atom "/") -> outputEnv env $ wrapParens $ makeOp env "/" args
       
             (Atom "=") -> outputEnv env $ wrapParens $ makeOp env "==" args
             (Atom "!=") -> outputEnv env $ wrapParens $ makeOp env "/=" args

             (Atom "&&") -> outputEnv env $ wrapParens $ makeOp env "&&" args
             (Atom "||") -> outputEnv env $ wrapParens $ makeOp env "||" args
             (Atom "!") -> outputEnv env $ wrapParens $ "not " ++ (showHsEnv env $ args !! 0)

             (Atom "include") -> addIncludes env $ showHsEnv env $ args !! 0
             
             (Atom "print") -> outputEnv env $ concat ["print ", showHsEnv env (args !! 0), "\n"]
             (Atom "defun") -> outputEnv env $ defun env args

             (Atom "if") -> outputEnv env $ concat ["if ", showHsEnv env (args !! 0), " then (", showHsEnv env (args !! 1), ") else (", showHsEnv env (args !! 2), ")"]

             (List _) -> outputEnv env $ concat (mapShowHs env (op:args))
             (Atom _) -> outputEnv env $ concat ["(", showHsEnv env (op), " ", concat (mapShowHs env args), ")"]
             (Number _) -> outputEnv env $ showHsEnv env op
             (String _) -> outputEnv env $ showHsEnv env op
             (Bool _) -> outputEnv env $ showHsEnv env op

codegen :: Env -> [LispVal] -> Env
codegen env xs = concatEnv $ mapShowHsEnv env xs
