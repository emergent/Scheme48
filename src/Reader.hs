module Reader where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
        | List [LispVal]
        | DottedList [LispVal] LispVal
        | Number Integer
        | Float Float    -- TODO(6): Float
        | String String
        | Character Char -- TODO(5): Charactor
        | Bool Bool
        deriving Show

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
    -- TODO(2): \"が文字列を終わりにせず、二重引用符のリテラル表現となるように
    -- TODO(3): \n, \r, \t, \\などのエスケープ文字も認識するように
    x <- many (escapeChars <|> noneOf "\"\\")
    -- x <- many (noneOf "\"")
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
parseNumber = (Number . read) <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
