module Codegen (codegen) where

import Struct

makeOp :: String -> [LispVal] -> String
makeOp op args = concat [codegen (args !! 0), " ", op, " ", codegen (args !! 1)]

wrapParens :: String -> String
wrapParens x = "(" ++ x ++ ")"

showC :: LispVal -> String
showC (Atom x) = x
showC (Number x) = show x
showC (String x) = "\"" ++ x ++ "\""
showC (Bool x) = if x then "true" else "false"
showC (List []) = ""
showC (List (op:args)) = case (op) of
  (Atom "+") -> makeOp "+" args
  (Atom "-") -> makeOp "-" args
  (Atom "*") -> makeOp "*" args
  (Atom "/") -> makeOp "/" args

  (Atom "=") -> makeOp "==" args
  (Atom "!=") -> makeOp "!=" args

  (Atom "add") -> makeOp "+" args
  (Atom "subtract") -> makeOp "-" args
  (Atom "multiply") -> makeOp "*" args
  (Atom "divide") -> makeOp "/" args

  (Atom "and") -> makeOp "&&" args
  (Atom "or") -> makeOp "||" args
  (Atom "not") -> makeOp "!" args
  
  (Atom "print") -> concat ["puts(", showC (args !! 0), ");"]
  (Atom "defun") -> concat [showC (args !! 1), " ", showC (args !! 0), "(){\n", showC (args !! 2), "\n}\n"]
  (Atom "type") -> (case (args !! 0) of
                      (Atom "i4") -> "int"
                      (Atom "i2") -> "short"
                      (Atom "bool") -> "bool"
                      )

  (Atom "if") -> concat ["if (", showC (args !! 0), ") {", showC (args !! 1), "}"]

  -- (Atom "include") -> includeFile (showC $ args !! 0)
  
  (List _) -> concat (map showC (op:args))
  _ -> "" 

showArgs :: LispVal -> String
showArgs (List xs) = " " ++ concat (map showArgs xs)
showArgs (Atom x) = x
showArgs _ = ""

showHs :: LispVal -> String
showHs (Atom x) = x
showHs (Number x) = show x
showHs (String x) = "\"" ++ x ++ "\""
showHs (Bool x) = if x then "true" else "false"
showHs (List []) = ""
showHs (List (op:args)) = case (op) of
       (Atom "+") -> makeOp "+" args
       (Atom "-") -> makeOp "-" args
       (Atom "*") -> makeOp "*" args
       (Atom "/") -> makeOp "/" args

       (Atom "=") -> wrapParens $ makeOp "==" args
       (Atom "!=") -> wrapParens $ makeOp "/=" args

       (Atom "&&") -> wrapParens $ makeOp "&&" args
       (Atom "||") -> wrapParens $ makeOp "||" args
       (Atom "!") -> wrapParens $ makeOp "!" args

       (Atom "print") -> concat ["putStrLn ", showHs (args !! 0)]
       (Atom "defun") -> concat [showHs (args !! 0), showArgs (args !! 1), " = " , showHs (args !! 2)]

       (Atom "if") -> concat ["if ", showHs (args !! 0), " then (", showHs (args !! 1), ") else (", showHs (args !! 2), ")"]

       (List _) -> concat (map showHs (op:args))
       (Atom _) -> concat ["(", showHs (op), " ", concat (map showHs args), ")"]
       (Number _) -> showHs op


codegen :: LispVal -> String
codegen = showHs