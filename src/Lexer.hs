module Lexer where

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

surround :: Parser a -> Parser a
surround p = do
         _ <- sc
         x <- p
         _ <- sc
         return x

expr :: Parser Stmt
expr = try pList
   <|> try pAtom
   <|> try pString
   <|> try pInt

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

pList :: Parser Stmt
pList = parens insideList
     where
      insideList = liftM List $ sepEndBy expr sc

pAtom :: Parser Stmt
pAtom = do
     first <- letter
     rest <- some $ letter <|> digit <|> symbol
     let x = first:rest
     return $ Atom $ x

pString :: Parser Stmt -- function name string is taken
pString = do
        xs <- between (char '"') (char '"') (many $ noneOf "\"")
        return $ String xs

pInt :: Parser Stmt
pInt = liftM (Number . read) $ some digit