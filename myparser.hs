module MyParser where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readHex)


main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving(Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of--- What does this mean?
    Left err -> "No match: " ++ show err
    Right val -> "Found Value " ++ show val

spaces :: Parser ()
spaces = skipMany1 space


escapedChar = do
    char '\\'
    x <- oneOf ['"', '\\', '\n', '\r', '\t']
    return $ case x of
        '\\' -> x
        '\"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChar <|> noneOf ['"', '\\']
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

isHex = do
    char '#'
    char 'x'
    hex <- many1 hexDigit
    let ((num,_):_) = readHex $ hex !! 0
    return num

parseNumber :: Parser LispVal
parseNumber = do
    nums <- isHex <|> many1 digit
    return $ Number (read nums)

--parseNumber = many1 digit >>= \nums -> return $ Number (read nums)


parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber








