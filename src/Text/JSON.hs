-- |
-- Module       : Text.JSON
-- Description  : JSON library
-- Copyright    : (c) Mattias Harrysson 2015
-- License      : LGPL
-- Stability    : experimental
-- Portability  : portable
--
module Text.JSON where

import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Numeric (readHex)
import Text.Parsec (ParseError, parse, try, string, sepBy, count)
import Text.Parsec.Char (char, digit, hexDigit, oneOf, noneOf)
import Text.Parsec.Combinator (eof, many1, option)
import Text.Parsec.String (Parser)

data JValue = JObject [(JValue, JValue)]
            | JArray [JValue]
            | JString String
            | JNumber Double
            | JBool Bool
            | JNull
            deriving (Eq, Show)

parseEof :: Parser a -> String -> Either ParseError a
parseEof p = parse (p <* eof) ""

parseWs :: Parser a -> String -> Either ParseError a
parseWs p = parseEof $ ws >> p

lexeme :: Parser a -> Parser a
lexeme p = p <* ws

-- Expression

valueE :: Parser JValue
valueE = try objectE
         <|> arrayE
         <|> stringT
         <|> numberT
         <|> boolT
         <|> nullT

objectE :: Parser JValue
objectE = do
    void $ lexeme $ char '{'
    e <- lexeme $ commaSep memberE
    void $ lexeme $ char '}'
    return $ JObject e

arrayE :: Parser JValue
arrayE = do
    void $ lexeme $ char '['
    e <- lexeme $ commaSep valueE
    void $ lexeme $ char ']'
    return $ JArray e

memberE :: Parser (JValue, JValue)
memberE = do
    name <- stringT
    void $ lexeme $ char ':'
    value <- valueE
    return $ (name, value)

-- Terminal

stringT :: Parser JValue
stringT = do
    void $ char '"'
    e <- many character
    void $ char '"'
    return $ JString e

numberT :: Parser JValue
numberT = do
    a <- option "" (string "-")
    b <- string "0" <|> nonZero
    c <- option "" frac
    d <- option "" expt
    return $ JNumber $ read $ concat [a, b, c, d]
  where
    nonZero = (:) <$> oneOf ['1'..'9'] <*> many digit
    frac    = (:) <$> char '.' <*> many1 digit
    expt    = do
        a <- oneOf "eE"
        b <- option '+' (oneOf "+-")
        c <- many1 digit
        return (a:b:c)

boolT :: Parser JValue
boolT = do
    e <- lexeme $ True <$ string "true" <|> False <$ string "false"
    return $ JBool e

nullT :: Parser JValue
nullT = do
    void $ lexeme $ string "null"
    return $ JNull

-- Helper

ws :: Parser ()
ws = void $ many $ oneOf " \n\t\r"

character :: Parser Char
character = try unicode <|> escape <|> noneOf "\"\\"

unicode :: Parser Char
unicode = do
    void $ string "\\u"
    e <- count 4 hexDigit
    return $ hexChar e
  where
    hexChar h = toEnum hex
      where [(hex, _)] = readHex h

escape :: Parser Char
escape = char '\\' *> oneOf "\"\\/\b\f\n\r\t"

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` comma)
  where
    comma = lexeme $ char ','
