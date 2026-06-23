module Codegen (codegen) where

import Struct

makeOp :: String -> [LispVal] -> String
makeOp op args = concat [codegen (args !! 0), " ", op, " ", codegen (args !! 1)]

wrapParens :: String -> String
wrapParens x = "(" ++ x ++ ")"

showArgs :: LispVal -> String
showArgs (List (x:xs)) = concat [" ", showHs x, concat (map showArgs xs)]
showArgs _ = ""

defunBody :: LispVal -> String
defunBody (List xs) = concat $ map (addTab . showHs) xs

defun :: [LispVal] -> String
defun args = concat [showHs (args !! 0), showArgs (args !! 1), defunTail args]
      where
        defunTail args = case (showHs $ args !! 0) of
                  "main" -> concat [" = do\n",  defunBody (args !! 2), "\n"]
                  _ -> concat [" = ", showHs (args !! 2), "\n"]

addTab :: String -> String
addTab x = (take 4 $ repeat ' ') ++ x

showHs :: LispVal -> String
showHs (Atom x) = x
showHs (Number x) = show x
showHs (String x) = "\"" ++ x ++ "\""
showHs (Bool x) = if x then "true" else "false"
showHs (List []) = ""
showHs (List (op:args)) = case (op) of
       (Atom "+") -> wrapParens $ makeOp "+" args
       (Atom "-") -> wrapParens $ makeOp "-" args
       (Atom "*") -> wrapParens $ makeOp "*" args
       (Atom "/") -> wrapParens $ makeOp "/" args

       (Atom "=") -> wrapParens $ makeOp "==" args
       (Atom "!=") -> wrapParens $ makeOp "/=" args

       (Atom "&&") -> wrapParens $ makeOp "&&" args
       (Atom "||") -> wrapParens $ makeOp "||" args
       (Atom "!") -> wrapParens $ "not " ++ (showHs $ args !! 0)

       (Atom "print") -> concat ["print ", showHs (args !! 0), "\n"]
       (Atom "defun") -> defun args

       (Atom "if") -> concat ["if ", showHs (args !! 0), " then (", showHs (args !! 1), ") else (", showHs (args !! 2), ")"]

       (List _) -> concat (map showHs (op:args))
       (Atom _) -> concat ["(", showHs (op), " ", concat (map showHs args), ")"]
       (Number _) -> showHs op       

codegen :: LispVal -> String
codegen = showHs