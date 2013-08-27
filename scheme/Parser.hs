{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)
import System.Environment ()
import Text.ParserCombinators.Parsec
    ( Parser
    , (<|>), char, digit, letter, many, many1, noneOf, oneOf, parse, sepBy
    , skipMany1, space, string
    )

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Zero or more characters enclosed in double-quotes.
parseString :: Parser LispVal
parseString = char '"' *> str <* char '"'
    where str = String <$> many (noneOf "\"")

-- A letter or symbol followed by any number of letters, digits, or symbols.
parseAtom :: Parser LispVal
parseAtom = parseAtom' >>=
            pure <$> \atom -> case atom of
                                  "#t" -> Bool True
                                  "#f" -> Bool False
                                  _    -> Atom atom
  where
    parseAtom' :: Parser String
    parseAtom' = (:) <$> first <*> rest
      where
        first = letter <|> symbol
        rest  = many $ letter <|> digit <|> symbol

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"
