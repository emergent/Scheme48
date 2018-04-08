module Reader where

import           Control.Monad
import           Control.Monad.Error
import           Error
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Value

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeChars :: Parser Char
escapeChars = do
    x <- char '\\'  >> oneOf "\\\"nrt"
    return $ case x of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _   -> x

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapeChars <|> noneOf "\"\\")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- TODO(4) Scheme standard for different basesもサポートするように
-- TODO(7) Schemeの数値型のfull numeric towerを実装するデータ型とパーサを書く
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit


parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do  char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val
