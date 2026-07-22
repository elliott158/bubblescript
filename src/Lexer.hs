module Lexer (parseCode) where

import Struct (Stmt(..))

import Control.Monad (void, liftM)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Error = Void
type Input = String

type Parser = Parsec Error Input

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

letter :: Parser Char
letter = oneOf $ ['a'..'z'] ++ ['A'..'Z']

digit :: Parser Char
digit = oneOf $ ['0'..'9']

sc :: Parser ()
sc = L.space (void spaceChar) lineC blockC
  where lineC = L.skipLineComment "--"
        blockC = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseCode :: Input -> Either (ParseErrorBundle Input Error) [Stmt]
parseCode = parse (exprs <* eof) "<lisp>"
-- parseCode = parse (sc *> exprs <* eof) "<lisp>"

exprs :: Parser [Stmt]
exprs = some expr

expr :: Parser Stmt
expr = try pList
   <|> try pAtom
   <|> try pString
   <|> try pInt

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

pList :: Parser Stmt
pList = parens insideList
     where
      insideList = liftM List $ sepEndBy expr sc

pAtom :: Parser Stmt
pAtom = do
     first <- letter <|> symbol
     rest <- some $ letter <|> digit <|> symbol
     let x = first:rest
     return $ Atom $ x

pString :: Parser Stmt -- function name string is taken
pString = String <$> between (char '"') (char '"') (many $ noneOf "\"")

pInt :: Parser Stmt
pInt = liftM (Number . read) $ some digit