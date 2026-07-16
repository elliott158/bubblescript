module Lexer where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Struct

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

spaces :: Parser ()
spaces = skipMany1 space

whitespaceChar :: Parser Char
whitespaceChar = (space <|> tab <|> newline)

whitespaces :: Parser ()
whitespaces = skipMany1 (whitespaceChar)

maybeWhitespace :: Parser ()
maybeWhitespace = skipMany (whitespaceChar)

parseSingleComment :: Parser String
parseSingleComment = do
    _ <- string "//"
    manyTill anyChar newline

parseMultiComment :: Parser String
parseMultiComment = do
  _ <- string "/*"
  manyTill anyChar (try (string "*/"))

parseSkipComments :: Parser ()
parseSkipComments = skipMany (parseSingleComment <|> parseMultiComment)

parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                x <- many (noneOf "\"")
                _ <- char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "true" -> Bool True
                         "false" -> Bool False
                         _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepEndBy parseExpr maybeWhitespace

parseFullList :: Parser LispVal
parseFullList = do
  _ <- char '('
  _ <- maybeWhitespace
  x <- parseList
  _ <- maybeWhitespace
  _ <- char ')'
  return x

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseFullList

parseExprs :: Parser [LispVal]
parseExprs = sepBy1 parseExpr maybeWhitespace